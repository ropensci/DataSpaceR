if [ "$TRAVIS_OS_NAME" == "linux"]
then
body='{
  "request": {
    "branch": "master",
    "message": "API Request from DataSpaceR Build"
  }
}'

curl -s -X POST \
  -H "Content-Type: application/json" \
  -H "Accept: application/json" \
  -H "Travis-API-Version: 3" \
  -H "Authorization: token $TRAVIS_API_TOKEN" \
  -d "$body" \
  https://api.travis-ci.org/repo/CAVDDataSpace%2FDataSpaceR.test/requests
fi

