#!/bin/bash

# Install Docker

sudo yum update -y
sudo yum install jq -y
sudo amazon-linux-extras install docker -y
sudo service docker restart
sudo mkdir pbf
sudo chmod o+w pbf

# Run Java environment, setup osm tools for snipping osm files and jar, get github repo

sudo docker run -itd --name java -v /home/ec2-user/Arlington-County-Food-Security/routing/data/pbf:/data openjdk:8-jdk /bin/bash
sudo docker exec java apt-get update -y
sudo docker exec java apt-get install osmctools -y

# Clip US file to bounding box for each metro. Used max/min lat/lon from "double buffer" block groups
# plus half degree buffer on each side to determine bounding box for each

#create bucket
bucket='arlington-county-food-security'
sudo docker exec java wget "http://download.geofabrik.de/north-america/us-latest.osm.pbf"


n=$(jq -r ".bounding.n" osm_bounds.json)
s=$(jq -r ".bounding.s" osm_bounds.json)
e=$(jq -r ".bounding.e" osm_bounds.json)
w=$(jq -r ".bounding.w" osm_bounds.json)

sudo docker exec java osmconvert us-latest.osm.pbf -b=${e},${s},${w},${n} --complete-ways -o=data/osm_bounds.pbf
# Send to S3
sudo aws s3 cp pbf/osm_bounds.pbf s3://${bucket}/util-files/geo-data/ --acl bucket-owner-full-control





