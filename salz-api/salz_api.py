from flask import Flask
from flask import jsonify
from flask_cors import CORS
from datetime import datetime
import psycopg2
import json
from pony.orm import *
import time

app = Flask(__name__)
CORS(app)


db = Database()


def wait_for_connection():
    retries = 0
    max_retries = 10

    while retries < max_retries:
        try:
            db.bind(provider='postgres', user='postgres', password='mysecretpassword', host='salz-db', port=5432, database='postgres')
        except pony.orm.dbapiprovider.OperationalError as error:
            print("Could not connect to postgres database. Retrying in 3 seconds")
            retries += 1
            time.sleep(3)

db.bind(provider='postgres', user='postgres', password='mysecretpassword', host='salz-db', port=5432, database='postgres')

# this is already pretty shittiliy organized anyways, so why not throw some constants up here eh

DEFAULT_TURNHISTORY = 100


set_sql_debug(True)

# classes corresponding to tables in the db
class Game(db.Entity):
    id = PrimaryKey(int, auto=True)
    turnid = Required(int)
    x = Required(int)
    y = Required(int)
    playerid = Required(int)
    generated_at = Required(datetime)

class Players(db.Entity):
    playerid = PrimaryKey(int, auto=True)
    username = Required(str)
    botdir = Optional(str)
    updatedbot = Required(bool)
    newbotdir = Optional(str)
    botstatus = Optional(str)

# so pony knows how the Game class relates to the Game table
db.generate_mapping()

# things accessing the db need to have the db_session decorator
@app.route('/')
@db_session
def hello_world():

    users = select(p.username for p in Players)[:]

    return f"{users}"

@app.route('/frames')
@db_session
def get_frames():
    maxFrame = db.select('* FROM get_latest_turnid()')[0]
    
    endFrame = maxFrame - DEFAULT_TURNHISTORY

    frames = db.select('* from get_frames($endFrame, $maxFrame)')

    jsonframes = [json.loads(x) for x in frames]

    response = app.response_class(
            response = json.dumps({"frames" : jsonframes}),
            status = 200,
            mimetype='application/json')
    return response


if __name__ == '__main__':
    app.debug = True
    app.run(host='0.0.0.0', port='8080')

