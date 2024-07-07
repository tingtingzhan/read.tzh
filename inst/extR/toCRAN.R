

if ('package:tzh' %in% search()) detach('package:tzh', unload = TRUE, character.only = TRUE)
# without detaching my \pkg{tzh}, my S3 methods will be \link[devtools]{document}ed as
# S3method(myS3generic,class)
# Tingting noted on 2023-05-25

devtools::load_all('../packageAdvanced')



