
#bucketlist()

####################################### try it with a single file
#put_object(
#  file = "F:\\EAA_clips\\st4_1hr_2002010101_2002070100_EAA_s3_test.txt", 
#  object = "st4_1hr_2002010101_2002070100_EAA_s3_test.txt", 
#  bucket = "stg4-eaa-clip",
#  multipart = TRUE
#)

#get_bucket(bucket = "stg4-eaa-clip")
# it's there, I can see it on the AWS GUI and download

########################################### read it into environment. can also do write_using
#s3read_using(FUN = read_csv, bucket = "stg4-eaa-clip", object = "st4_1hr_2002010101_2002070100_EAA_s3_test.txt")   

############################################ pull it down to local disk
#save_object(
#  object = "st4_1hr_2002010101_2002070100_EAA_s3_test.txt",
#  bucket = "stg4-eaa-clip",
#  file = "F:\\g2_test\\s3baby.txt"
#)


############################################ delete individual file from bucket
#delete_object(
#  object = "st4_1hr_2002010101_2002070100_EAA_s3_test.txt",
#  bucket = "stg4-eaa-clip"
#)

####### let's put the put_object in a loop
# try it with a single file

eaa_mpe<-list.files("F:\\EAA_clips")

for (r in eaa_mpe) {

put_object(
  file = paste0("F:\\EAA_clips\\",r), 
  object = r, 
  bucket = "stg4-eaa-clip",
  multipart = TRUE
)

}

do.call(rbind,get_bucket(bucket = "stg4-eaa-clip"))
