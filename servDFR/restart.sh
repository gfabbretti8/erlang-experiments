docker network disconnect $1 $2
docker container restart $2
docker network connect $1 $2
echo "done"
