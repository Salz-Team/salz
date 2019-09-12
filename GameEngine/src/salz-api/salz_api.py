from flask import Flask
from datetime import datetime
import psycopg2
from pony.orm import *

app = Flask(__name__)

db = Database()
db.bind(provider='postgres', user='postgres', password='mysecretpassword', host='localhost', port=5432, database='postgres')

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

if __name__ == '__main__':
    app.debug = True
    app.run(host='0.0.0.0', port='8080')

