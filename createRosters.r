createRosters = function(inputFilePath = readline('Example: c:/users/acer/downloads/'),
                         sessionStartDate = readline('Example: "2023-04-16"'),
                         RMDFile = 'c:/users/acer/downloads/testRosterhtml.Rmd',
                         outputFilePath = readline('Example: c:/users/acer/downloads/')){

  require(dplyr)
  
  registrations = read.csv(paste0(inputFilePath, '/Class Registrations.csv'))
  vaccines = read.csv(paste0(inputFilePath, '/Class Registration Vaccine List.csv'))
  waivers = read.csv(paste0(inputFilePath, '/Client Waivers.csv'))
  schedule = read.csv(paste0(inputFilePath, '/Class Schedule.csv'))
  doglist = read.csv(paste0(inputFilePath, 'Dog List.csv'))

  # Calculate dog age
  doglist = doglist %>%
    dplyr::mutate(.,
                  EstAge = ifelse(.$Birth.Date == '1/1/0001 12:00:00 AM',
                                  paste0('>', round(as.numeric(lubridate::ymd_hms(Sys.time()) -
                                  lubridate::mdy_hms(.$Signed.Up))/(360*24), 1), 'yr'),
                                paste0(round(as.numeric(lubridate::ymd_hms(Sys.time()) -
                                        lubridate::mdy_hms(.$Birth.Date))/360, 1),'yr')))

  # Organize registrations
  tmp2 = registrations%>%
         subset(., 
                lubridate:::mdy_hms(`Start.Date`) >=
                lubridate::ymd_hms(paste(sessionStartDate, '00:00:00'))) %>%
         dplyr::mutate(., 
                EstAge = doglist$EstAge[match(paste(.$First,.$Last,.$Dog),
                                              paste(doglist$First,doglist$Last,doglist$Name))],
                Breed = ifelse(.$Secondary.Breed != '-',
                               paste(.$Primary.Breed,.$Secondary.Breed,
                              sep = '/'), 
                              .$Primary.Breed),
                Sex = gsub(' ', '', gsub('Neutered','N',
                           gsub('Spayed', 'S', 
                                gsub('Female', 'F', 
                                     gsub('Male', 'M', .$Gender)))))) %>%
         dplyr::mutate(.,
                       DogDetails = paste(.$Dog,
                                    '(',
                                   ifelse(!grepl('>', .$EstAge), .$EstAge, ''),
                                   .$Sex,
                                   .$Breed, ')'),
                Client = paste(.$First, .$Last),
                Payment = ifelse(.$Owed == .$Paid,
                                 'X',
                                 ifelse(.$Owed > .$Paid,
                                         paste0('Owes $', .$Owed - .$Paid),
                                         paste0('Overpaid $', abs(.$Owed-.$Paid))))) %>%
          dplyr::select(., Course.Title, Start.Date, 
                           Client, DogDetails, Payment,
                        First, Last,Dog,
                        Signed.Up) %>%
          dplyr::mutate(., SignedUpDate = substr(as.character(lubridate::mdy_hms(.$Signed.Up)),1,10))

  # Organize vaccines
  


fecal = vaccines %>%
  subset(., Vaccine == 'Fecal')%>%
  dplyr::group_by(., Start.Date, Course.Title, Owner, Dog) %>%
  dplyr::slice(which.max(lubridate::mdy_hms(Expires)))
rabies = vaccines %>%
  subset(., Vaccine == 'Rabies') %>%
  dplyr::group_by(., Start.Date, Course.Title, Owner, Dog) %>%
  dplyr::slice(which.max(lubridate::mdy_hms(Expires)))
vx = merge(fecal, rabies, by = c('Start.Date','Course.Title','Owner','Dog'),
           all = TRUE)
vx = vx %>%
  dplyr::mutate(., 
                Fecal = ifelse(!is.na(Expires.x),
                               'Y','N'),
                Rabies = ifelse(!is.na(Expires.y),
                                ifelse(lubridate::mdy_hms(Expires.y) <=
                                         lubridate::ymd_hms(Sys.time()),
                                       'Exp', 'Y'), 'N'))

tmp3 = merge(tmp2, 
              vx,
              by.x = c('Client','Course.Title','Start.Date','Dog'),
              by.y = c('Owner','Course.Title','Start.Date','Dog'),
              all.x = TRUE) %>%
  dplyr::mutate(.,
                Fecal = ifelse(is.na(.$Fecal),'',.$Fecal),
                Rabies = ifelse(is.na(.$Rabies), '', .$Rabies))

head(waivers)
waivers = waivers %>%
  subset(., Waiver.Name == 'AGREEMENT, RELEASE, WAIVER OF LIABILITY AND ASSUMPTION OF RISK - Group class') %>% 
  group_by(., First, Last, Waiver.Type, Sign.Up.Date) %>%
  slice(which.max(lubridate::mdy_hms(Date.Completed))) %>%
  ungroup(.) %>%
  dplyr::mutate(., SignedUpDate = substr(as.character(lubridate::mdy_hms(.$Date.Completed)),1,10))

tmp4 = merge(tmp3, waivers,
              by.x = c('First','Last','SignedUpDate'),
              by.y = c('First','Last','SignedUpDate'),
              all.x = TRUE) %>%
  dplyr::mutate(.,
                Waiver.Type = ifelse(is.na(.$Waiver.Type),
                                     '', .$Waiver.Type))


tmp5 = tmp4 %>%
  dplyr::select(., Course.Title, Start.Date,
                Client, DogDetails,
                Payment, Fecal, Rabies, Waiver.Type) %>%
  dplyr::mutate(., Waiver.Type = ifelse(is.na(.$Waiver.Type), '',
                          .$Waiver.Type)) %>%
  dplyr::rename(., Waiver = Waiver.Type) %>%
  dplyr::arrange(., Course.Title, Start.Date, Client) %>%
  dplyr::mutate(., 
                '1' = '', '2' = '',
                '3' = '', '4' = '',
                '5' = '', '6' = '')

schedule = schedule[!duplicated(schedule),]
schedule = schedule %>%
  dplyr::mutate(.,
                StartDate = substr(as.character(lubridate::mdy_hms(.$Start.Date)), 1,10),
                EndDate = substr(as.character(lubridate::mdy_hms(.$Start.Date) + (7 * 6 * 24 * 60 * 60)), 1,10))

schedule = schedule %>%
  dplyr::mutate(.,
                Day = weekdays(lubridate::mdy_hms(.$Start.Date)),
                StartTime = ifelse(as.numeric(substr(as.character(lubridate::mdy_hms(.$Start.Date)), 12,13)) < 13,
                                   as.numeric(substr(as.character(lubridate::mdy_hms(.$Start.Date)), 12,13)),
                                   as.numeric(substr(as.character(lubridate::mdy_hms(.$Start.Date)), 12,13))-12),
                AMPM = ifelse(grepl('PM', .$Start.Date), 'PM','AM'),
                On30 = ifelse(grepl(':30:00', .$Start.Date),
                              'Y','N')) %>%
  dplyr::mutate(.,
                Detail = paste0(.$Day,
                                ' ', .$StartTime,
                                ifelse(.$On30 == 'Y', ':30',''),'-',
                                .$StartTime+1,
                ifelse(.$On30 == 'Y', ':30',''),.$AMPM,' --- ',
                                .$Course.Title,
                                ' --- (',
                                .$StartDate, ' - ', .$EndDate, ') --- ',
                                '$', .$Deposit, ' ($', .9*.$Deposit, ' rescue)')) %>%
  dplyr::mutate(., ID = paste(.$StartDate, .$Course.Title))
schedule$Order = 1:nrow(schedule)

tmp5$StartDate = substr(as.character(lubridate::mdy_hms(tmp5$Start.Date)), 1,10)
tmp5$ID = paste(tmp5$StartDate, tmp5$Course.Title)

# Match schedule to registration
# Need to handle duplicates
(dupes = schedule %>% plyr::ddply(., ~ ID, plyr::summarize, ct = length(Day)) %>%
  subset(., ct > 1))
schedule$ID[schedule$ID == '2023-04-20 Beginning/Intermediate Agility' &
            schedule$StartTime == 5] = paste(schedule$ID[schedule$ID == '2023-04-20 Beginning/Intermediate Agility' &
                                                           schedule$StartTime == 5], '1')

schedule$ID[schedule$ID == '2023-04-20 Beginning/Intermediate Agility' &
              schedule$StartTime == 7] = paste(schedule$ID[schedule$ID == '2023-04-20 Beginning/Intermediate Agility' &
                                                             schedule$StartTime == 7], '2')

tmp5[tmp5$ID == '2023-04-20 Beginning/Intermediate Agility' &
        grepl('Reisch|Switzenberg|Paige|Langer|Marquiss|Dziekonski|Berry',
              tmp5$Client),
      'ID'] = paste(tmp5[tmp5$ID == '2023-04-20 Beginning/Intermediate Agility' &
                            grepl('Reisch|Switzenberg|Paige|Langer|Marquiss|Dziekonski|Berry',
                                  tmp5$Client),
                          'ID'], '1')

tmp5[tmp5$ID == '2023-04-20 Beginning/Intermediate Agility' &
        grepl('Kauffman|Messa|Weber|Ehler|Webb|Ben-David',
              tmp5$Client),
      'ID'] = paste(tmp5[tmp5$ID == '2023-04-20 Beginning/Intermediate Agility' &
                            grepl('Kauffman|Messa|Weber|Ehler|Webb|Ben-David',
                                  tmp5$Client),
                          'ID'], '2')

out = merge(tmp5,
              schedule[,c('ID','Detail','Order')],
              by = 'ID', all.x = TRUE)

out = out %>%
  dplyr::mutate(., Day = weekdays(lubridate::mdy_hms(.$Start.Date))) %>%
  dplyr::arrange(., Order, Client)

saveRDS(out, paste0(outputFilePath, '/rosterData_',
        gsub('-','',Sys.Date()), '.RDS'))

rmarkdown::render(RMDFile, output_file = paste0('Rosters_',
                                                gsub('-','',Sys.Date()), '.html'))

return(out)

}
