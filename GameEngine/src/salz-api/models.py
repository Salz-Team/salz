from flask_sqlalchemy import SQLAlchemy

from . import app

db = SQLAlchemy(app)

class GameModel(db.Model):
    __tablename__ = 'game'
    id = db.Column(db.Integer, primary_key=True)
    turnId = db.Column(db.Integer)
    x = db.Column(db.Integer)
    y = db.Column(db.Integer)
    playerid = db.Column(db.Integer)
    timestamp = db.Column(db.DateTime)


class PlayerModel(db.Model):
    playerid = db.Column(db.Integer, primary_key=True)
    username = db.Column(db.String(50))
    currentBotDir = db.Column(db.String(200))
    previousBotDir = db.Column(db.String(200))
    botstatus = db.Column(db.String(8))

