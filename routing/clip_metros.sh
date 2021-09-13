#!/bin/bash

# Install Docker

sudo yum update -y
sudo yum install jq -y
sudo amazon-linux-extras install docker -y
sudo service docker restart
sudo mkdir pbf
sudo chmod o+w pbf

# Run Java environment, setup osm tools for snipping osm files and jar, get github repo

sudo docker run -itd --name java -v /home/ec2-user/mcig-access-opportunity/otp/src/otp-setup/pbf:/data openjdk:8-jdk /bin/bash
sudo docker exec java apt-get update -y
sudo docker exec java apt-get install osmctools -y

# Clip US file to bounding box for each metro. Used max/min lat/lon from "double buffer" block groups
# plus half degree buffer on each side to determine bounding box for each

names='seattle lansing baltimore nashville'
bucket='access-opportunity/code-review'
sudo docker exec java wget "http://download.geofabrik.de/north-america/us-latest.osm.pbf"

for name in $names
do
    n=$(jq -r ".$name.bounding.n" clip_metros_params.json)
    s=$(jq -r ".$name.bounding.s" clip_metros_params.json)
    e=$(jq -r ".$name.bounding.e" clip_metros_params.json)
    w=$(jq -r ".$name.bounding.w" clip_metros_params.json)
    
    sudo docker exec java osmconvert us-latest.osm.pbf -b=${e},${s},${w},${n} --complete-ways -o=data/${name}_metro.pbf
    # Send to S3
    sudo aws s3 cp pbf/${name}_metro.pbf s3://${bucket}/util-files/geo-data/ --acl bucket-owner-full-control
done




