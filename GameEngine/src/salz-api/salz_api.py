from flask import Flask
from flask_sqlalchemy import SQLAlchemy
import psycopg2

app = Flask(__name__)
app.config['SQLALCHEMY_DATABASE_URI'] = "postgresql://postgres:mysecretpassword@localhost:5432"

# put this somewhere safe later
app.config['SQLALCHEMY_DATABASE_URI'] = "postgresql://postgres:mysecretpassword@localhost:5432"

db = SQLAlchemy(app)

class Game(db.Model):
    __tablename__ = 'game'
    id = db.Column(db.Integer, primary_key=True)
    turnId = db.Column(db.Integer)
    x = db.Column(db.Integer)
    y = db.Column(db.Integer)
    playerid = db.Column(db.Integer)
    generated_at = db.Column(db.DateTime)


class Players(db.Model):
    playerid = db.Column(db.Integer, primary_key=True)
    username = db.Column(db.VARCHAR)
    botdir = db.Column(db.VARCHAR)
    updatedbot = db.Column(db.BOOLEAN)
    newbotdir = db.Column(db.VARCHAR)
    botstatus = db.Column(db.VARCHAR)

@app.route('/')
def hello_world():

    testthing = Players.query.all()

    users = []
    print(testthing)
    for p in testthing:
        print(p.username)
        users.append(p.username)

    return f"the current users are {users}"


if __name__ == '__main__':
    app.debug = True
    app.run(host='0.0.0.0', port='8080')

