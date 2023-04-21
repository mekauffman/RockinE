processVenmo = function(inputFilePath = readline('Example: c:/users/acer/downloads/'),
                        outputFilePath = readline('Example: c:/users/acer/downloads/'))
{
  
   
  require(dplyr)

  print('Reading Venmo file(s)...')
  venmoFile = list.files(inputFilePath,
                         pattern = 'venmo', full.names = TRUE,
                         ignore.case = TRUE)
  
  venmo = do.call("rbind",
                  lapply(venmoFile, function(x){
                  tmp = as.data.frame(readxl::read_excel(x))
                  whichtmp = which(tmp[,2] == 'ID')
                  colnames(tmp) = tmp[whichtmp,]
                  tmp = tmp[(whichtmp+1):nrow(tmp),2:ncol(tmp)]
                 }))
  
  print('Processing file(s)...')

  processFile = function(venmofile){
  
    venmofile$Disclaimer =  NULL
  
    whichAreWithdrawals = which(venmofile$Type == 'Standard Transfer')
    withdrawals = venmofile[whichAreWithdrawals,]
    
    starts = c(1, whichAreWithdrawals[1:(length(whichAreWithdrawals)-1)]+1) 
    ends = c(whichAreWithdrawals-1)
  
    withdrawals$WithdrawalDateTime = lubridate::ymd_hms(unlist(lapply(strsplit(withdrawals$Datetime, 'T'), paste, collapse = ' ')))
    
    aWithdrawal = function(x){
      
    # Summarize what happened between last withdrawal and this one
    test = venmofile[starts[x]:ends[x],] %>%
      dplyr::mutate(., ID = 1:nrow(.),
                       Name = .$From,
                       'Dog/Class' = .$Note,
                       Gross = as.numeric(gsub(' |\\+|\\$', '', .$'Amount (total)')) ,
                       # Fee = - round((0.019 * as.numeric(gsub(' |\\+|\\$', '', .$'Amount (total)'))) + 0.10, 2),
                       Fee = -round(as.numeric(.$'Amount (fee)'),2),
                       Net = as.numeric(gsub(' |\\+|\\$', '', .$'Amount (net)')))
                       # Final = as.numeric(gsub(' |\\+|\\$', '', .$'Amount (total')) - as.numeric(gsub(' |\\+|\\-|\\$', '', .$'Amount (fee)'))) 
  
      test = gtools::smartbind(test[!is.na(test$Gross),c('ID','Name','Dog/Class','Gross','Fee','Net')],
                                data.frame('Name' = paste('Withdrawal', withdrawals$WithdrawalDateTime[x]),
                                'Net' = as.numeric(gsub(' |\\+|\\$|-', '', withdrawals$'Amount (total)'[x]))))
      
      test$Net[nrow(test)] = -test$Net[nrow(test)]
      return(test)  
  } # end a withdrawal
  
  out = lapply(1:nrow(withdrawals), aWithdrawal)
  
  return(out)

  }

  tmpOut = processFile(venmo)

  print('Writing CSV(s) to outputFilePath...')
  lapply(1:length(tmpOut),
         function(x){
           write.csv(tmpOut[[x]],
            paste0(outputFilePath, '/VenmoWithdrawal_',
                   gsub('Withdrawal |[0-9]*:[0-9]*:[0-9]*| |-', '', tmpOut[[x]]$Name[nrow(tmpOut[[x]])]),
            '.csv'))})
}