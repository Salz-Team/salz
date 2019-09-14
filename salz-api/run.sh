echo "Ghetto way to wait for database: sleeping for 3s before running the api server"
sleep 5
pipenv run python salz_api.py
