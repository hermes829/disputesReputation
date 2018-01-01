# Need to figure out how to change source options
# Then to download file as .txt
# For now LexNex ICSID information gathered manually

# ####
# if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
# 	source('~/Research/disputesReputation/RCode/setup.R') }
# ####

# loadPkg('RSelenium')
# RSelenium::checkForServer()
# RSelenium::startServer()
# remDr <- remoteDriver(remoteServerAddr = "localhost" 
#                       , port = 4444
#                       , browserName = "firefox"
#                       )
# remDr$open()
# remDr$navigate("http://proxy.lib.duke.edu/login?url=http://www.lexisnexis.com/hottopics/lnacademic/?")

# # Input username
# webElem <- remDr$findElement(using = "xpath", "//*/input[@id = 'j_username']")
# webElem$sendKeysToElement(list("sfm12"))

# # Input password
# webElem <- remDr$findElement(using = 'xpath', "//*/input[@id = 'j_password']")
# webElem$sendKeysToElement(list('567coAT%4JAsk$', "\uE007"))

# # Navigate lexis nexus
# remDr$switchToFrame('mainFrame')

# # Enter search terms
# # webElem <- remDr$findElement(using = 'xpath', "//*[@id='terms']")
# webElem <- remDr$findElement('id', "terms")
# webElem$sendKeysToElement(list('icsid', '\uE007'))