from flask import Flask, request
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
    succeeded = False

    while (retries < max_retries) and not succeeded:
        try:
            db.bind(provider='postgres', user='postgres', password='mysecretpassword', host='salz-db', port=5432, database='postgres')
            succeeded = True
            return True
        except pony.orm.dbapiprovider.OperationalError as error:
            print("Could not connect to postgres database. Retrying in 3 seconds")
            retries += 1
            time.sleep(3)

    print("Couldn't connect to the database: max retries exceeded")
    exit(1)

wait_for_connection()

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
@app.route('/frames')
@db_session
def get_frames():
    args = request.args
    
    print(args)

    if ('startframe' in args) and ('endframe' in args):
        startFrame = args['startframe']
        endFrame = args['endframe']
    elif ('numframes' in args):
        endFrame = db.select('* FROM get_latest_turnid()')[0]
        startFrame = endFrame - args['numframes']

    else:
        endFrame = db.select('* FROM get_latest_turnid()')[0]
        startFrame = endFrame - DEFAULT_TURNHISTORY

    frames = db.select('* from get_frames($startFrame, $endFrame)')

    # pony not decoding the json, so I guess I gotta.
    jsonframes = [json.loads(x) for x in frames]

    response = app.response_class(
            response = json.dumps({"frames" : jsonframes}),
            status = 200,
            mimetype='application/json')
    return response

if __name__ == '__main__':
    app.debug = True
    app.run(host='0.0.0.0', port='8080')
