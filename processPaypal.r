processPaypal = function(inputFilePath = readline('Example: c:/users/acer/downloads/'),
                         outputFilePath = readline('Example: c:/users/acer/downloads/'))
  {
  
  
  require(dplyr)

  print('Reading CSV...')
  pp = read.csv(paste0(inputFilePath, '/Download.csv'))

  print('Processing file...')
  processFile = function(ppfile){
  
    # Organize file
    ppfile = ppfile %>%
      dplyr::mutate(., 
                    DT = lubridate::mdy_hms(paste(.$Date,.$Time))) %>%
      dplyr::arrange(., DT)
    
    # Identify which entries are withdrawals
    whichAreWithdrawals = which(ppfile$Type == 'General Withdrawal')
    withdrawals = ppfile[whichAreWithdrawals,]
    
    # Identify which entries are transactions associated with those withdrawals
    starts = c(1, whichAreWithdrawals[1:(length(whichAreWithdrawals)-1)]+1) 
    ends = c(whichAreWithdrawals-1)
  
    # Clean up date/time
    withdrawals$WithdrawalDateTime = lubridate::mdy_hms(paste(withdrawals$Date,
                                                              withdrawals$Time))
    
    # Function to process each withdrawal
    aWithdrawal = function(x){
      
      # Summarize what happened between last withdrawal and this one
      tmp = ppfile[starts[x]:ends[x],] %>%
        dplyr::mutate(., 
                      Gross = as.numeric(.$Gross),
                      Fee = as.numeric(.$Fee),
                      Net = as.numeric(.$Net)) %>%
        dplyr::mutate(., Final = .$Gross - .$Fee) %>%
        dplyr::mutate(., Details = ifelse(is.na(.$`Item.Title`), .$Note, .$`Item.Title`)) 
      
      # Get non payment transactions
      tmpb = tmp %>% 
        subset(., !Type %in% c('General Payment','Mobile Payment'))
      
      # Pull out "other" transactions
      tmpOther = tmp %>%
        subset(., Type %in% c('General Payment','Mobile Payment'), 
               c('Item.ID','Gross','Fee','Net','Name','Details'))
      
      # Check to see if each website payment has all three stages of payment and flag any that don't
      tmp2 = tmpb %>%
        subset(., !Type %in% c('Mobile Payment', 'General Payment')) %>%
        plyr::ddply(., ~ Item.ID, plyr::summarize,
                    Paid = ifelse(any(Type %in% c('Website Payment','Mobile Payment','General Payment')),
                                  1, 0),
                    Held = ifelse(any(Type %in% c('Payment Hold')),
                                  1, 0),
                    Released = ifelse(any(Type %in% c('Payment Release')),
                                      1, 0)) %>%
        dplyr::mutate(., Sum = .$Paid + .$Held + .$Released) %>%
        subset(., Sum == 1 & Released == 1) %>%
        dplyr::select(., Item.ID)
      
      # Pull in any information from transactions that started before the previous withdrawal
      # but didn't finish before it
      tmp3 = ppfile[1:(starts[x]-1),] %>%
        subset(., Item.ID %in% tmp2$Item.ID) %>%
        dplyr::mutate(., Gross = as.numeric(.$Gross)) %>%
        dplyr::mutate(., Fee = as.numeric(.$Fee)) %>%
        dplyr::mutate(., Net = as.numeric(.$Net)) %>%
        dplyr::mutate(., Final = .$Gross - .$Fee) %>%
        dplyr::mutate(., Details = ifelse(is.na(.$`Item.Title`), .$Notes, .$`Item.Title`)) 
      
      
      # Combine everything together
      tmp4 = rbind(tmpb, tmp3)
      
      deets = unique(tmp4[!is.na(tmp4$Name) & tmp4$Name != '',
                           c('Name','Item.ID','Details')])
      
      
      # Summarize for final output
      tmp6 = tmp4 %>%
        plyr::ddply(.,
                    ~ Item.ID,
                    plyr::summarize,
                    Gross = max(Gross),
                    Fee = min(Fee),
                    Net = sum(Net)) %>%
        subset(., Net != 0) %>%
        dplyr::mutate(., Name = deets$Name[match(.$Item.ID, deets$Item.ID)]) %>%
        dplyr::mutate(., Details = deets$Details[match(.$Item.ID, deets$Item.ID)])
      
      sum(tmp6$Net)
      
      
      tmp7 = rbind(tmp6, tmpOther)
      sum(tmp7$Net)
      
      
      # Format for Linda
      tmp8 = tmp7 %>%
        dplyr::mutate('PP #' = 1:nrow(.))
      tmp8$Class = unlist(lapply(tmp8$Details, function(x) unlist(strsplit(x, '-'))[1]))
      tmp8$SessionStart = gsub(' ', '', gsub('\\@.*','',unlist(lapply(tmp8$Details, function(x) unlist(strsplit(x, '-'))[3]))))
      tmp8$Dog = unlist(lapply(tmp8$Details, function(x) unlist(strsplit(x, '-'))[4]))
      tmp8 = tmp8[c('PP #','Name','Dog','Class','SessionStart','Gross','Fee','Net')]
      
      tmp9 = gtools::smartbind(tmp8,
                                data.frame('Name' = paste('Withdrawal', withdrawals$WithdrawalDateTime[x]),
                                           'Net' = withdrawals$Net[x]))
      
      return(tmp9)  
    } # end a withdrawal
  
    out = lapply(1:nrow(withdrawals),
                 aWithdrawal)
    
    return(out)
  
  }
  
  
  tmpOut = processFile(pp)
  
  print('Writing Paypal Withdrawal CSV(s) to outputFilePath')
  lapply(1:length(tmpOut),
         function(x){
           write.csv(tmpOut[[x]],
             paste0(outputFilePath,
             'PayPalWithdrawal_',
                    gsub('Withdrawal |[0-9]*:[0-9]*:[0-9]*| |-', '', 
                         tmpOut[[x]]$Name[length(tmpOut[[x]]$Name)]),
            '.csv'))
           })
  }
