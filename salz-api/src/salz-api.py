from flask import Flask, request, abort, url_for, session
from flask import jsonify, redirect
from flask_cors import CORS
import datetime
import psycopg2
import json
from pony.orm import *
import os
import shutil
import time
from authlib.flask.client import OAuth
import jwt
from urllib.parse import quote
from functools import wraps

app = Flask(__name__)
app.secret_key = "verysecret"
CORS(app)
oauth = OAuth(app)


# Load up env vars

PGDB = os.getenv("POSTGRES_DB", "postgres")
PGUSER = os.getenv("POSTGRES_USER", "postgres")
PGPASS = os.getenv("POSTGRES_PASSWORD", "mysecretpassword")
PGPORT = os.getenv("POSTGRES_PORT", "5432")
PGHOST = os.getenv("POSTGRES_HOST", "localhost")

WEBHOST = os.getenv("WEB_HOST", "http://localhost:3000")

BOTLOCATION = os.getenv("BOTLOCATION", ".")



oauth.register(
    name='github',
    client_id=os.getenv("GITHUB_CLIENT"),
    client_secret=os.getenv("GITHUB_SECRET"),
    client_kwargs={
            'scope': 'user:email',
            'token_placement' : 'header',
            'token_endpoint_auth_method': 'client_secret_basic'},
            #},
    api_base_url='https://api.github.com/',
    access_token_url='https://github.com/login/oauth/access_token',
    authorize_url='https://github.com/login/oauth/authorize'

)


def jwtobject(header):
    schema, token = header.split(' ')
    return jwt.decode(token, app.secret_key)

def jwt_auth(f):
    @wraps(f)
    def decorated(*args, **kwargs):
        authHeader = request.headers.get('Authorization')

        if authHeader:
            try:
                decoded = jwtobject(authHeader)
                print("all gucci")
            except jwt.ExpiredSignatureError:
                return "Expired token"
            except jwt.InvalidSignatureError:
                return "Invalid token"
            except Error as e:
                print(e)
        else:
            return "Token required"
        return f(*args, **kwargs)
    return decorated

@app.route('/user')
@jwt_auth
def userdata():
    # already been verified by jwt_auth
    authHeader = request.headers.get('Authorization')
    payload = jwt.decode(authHeader.split(' ')[1], app.secret_key)

    return payload

@app.route('/user/upload', methods=['POST'])
@jwt_auth
@db_session
def userupload():

    f = request.files['bot']

    # get username from JWT, then playerid
    obj = jwtobject(request.headers.get('Authorization'))
    username = obj['login']
    playerid = select(p.playerid for p in Players if p.username == username )[:][0]

    # 1. nuke the old folder (if exists) and remake
    newbotLocation = os.path.join(BOTLOCATION, 'tarbots', f'{playerid}', 'newbot')
    if os.path.exists(newbotLocation):
        shutil.rmtree(newbotLocation)
    os.makedirs(newbotLocation, exist_ok=True)

    # 2. Save the new file
    f.save(os.path.join(newbotLocation, "bot.tar.gz"))

    # 3. Update the database
    Players[playerid].newbotdir = os.path.join(newbotLocation, "bot.tar.gz")
    Players[playerid].updatedbot = True

    return ""

@app.route('/login')
def login():

    requestclient = request.args.get('client')

    if requestclient:
        print("web client detected")
        redirect_uri = url_for('authorized_webredirect', _external = True)
    else:
        redirect_uri = url_for('authorized', _external = True)

    return oauth.github.authorize_redirect(redirect_uri=redirect_uri)

@app.route('/login/auth/web')
def authorized_webredirect():
    token = oauth.github.authorize_access_token()
    resp = oauth.github.get('user')
    profile = resp.json()

    # create new Player object in database
    with db_session:
        if len(select(p for p in Players if p.username == profile['login'])) == 0:
            db.insert("players", username = profile['login'], updatedbot = False)

        playerDB = select(p for p in Players if p.username == profile['login']).first()

    payload = {
            "login" : profile['login'],
            "id" : playerDB.playerid,
            "email" : profile['email'],
            "exp" : datetime.datetime.utcnow() + datetime.timedelta(hours=12)
            }

    jwtoken = jwt.encode(payload, app.secret_key)
    return redirect(WEBHOST + "/login?jwt=" + quote(jwtoken)) 
    #return "foobar"

@app.route('/login/auth')
def authorized():
    token = oauth.github.authorize_access_token()
    resp = oauth.github.get('user')
    profile = resp.json()

    # create new Player object in database
    with db_session:
        if len(select(p for p in Players if p.username == profile['login'])) == 0:
            db.insert("players", username = profile['login'], updatedbot = False)

        playerDB = select(p for p in Players if p.username == profile['login']).first()

    payload = {
            "login" : profile['login'],
            "id" : playerDB.playerid,
            "email" : profile['email'],
            "exp" : datetime.datetime.utcnow() + datetime.timedelta(hours=12)
            }

    jwtoken = jwt.encode(payload, app.secret_key)
    return jsonify({'token' : jwtoken.decode('UTF-8')})


DEFAULT_TURNHISTORY = 100

db = Database()

def wait_for_connection():
    retries = 0
    max_retries = 10
    succeeded = False

    while (retries < max_retries) and not succeeded:
        try:
            db.bind(provider='postgres', user=PGUSER, password=PGPASS, host=PGHOST, port=PGPORT, database=PGDB)
            succeeded = True
            return True
        except pony.orm.dbapiprovider.OperationalError as error:
            print("Could not connect to postgres database. Retrying in 3 seconds")
            retries += 1
            time.sleep(3)

    print("Couldn't connect to the database: max retries exceeded")
    exit(1)

wait_for_connection()

# TODO add debug env vars to set this
set_sql_debug(True)

# classes corresponding to tables in the db
class Game(db.Entity):
    id = PrimaryKey(int, auto=True)
    turnid = Required(int)
    x = Required(int)
    y = Required(int)
    playerid = Required(int)
    generated_at = Required(datetime.datetime)

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

    latest_turnid = db.select('* FROM get_latest_turnid()')[0]

    # validate some shit

    # Make sure there's gamedata, if not, return empty frames
    if latest_turnid == None:
        response = app.response_class(
                response = json.dumps({"frames" : []}),
                status = 200,
                mimetype='application/json')
        return response

    # Check for query string args
    if ('startframe' in args) and ('endframe' in args):

        try:
            sf = int(args['startframe'])
            ef = int(args['endframe'])
        except ValueError as e:
            return abort(400)

        startFrame = sf
        endFrame = ef
    elif ('numframes' in args):

        try:
            nf = int(args['numframes'])
        except ValueError as e:
            return abort(400)

        endFrame = latest_turnid
        startFrame = endFrame - nf

    else:
        endFrame = latest_turnid
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
