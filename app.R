install.packages('rsconnect')
rsconnect::setAccountInfo(name='tatis02',
			  token='B7539D6DC941BFCCD6BEE2C97A13F1A0',
			  secret='<SECRET>')
        
 library(rsconnect)
    rsconnect::deployApp('tatis02/tatis02.github.io')
