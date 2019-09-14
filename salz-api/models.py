from flask_sqlalchemy import SQLAlchemy

from salz_api import app

app.config['SQLALCHEMY_DATABASE_URI'] = "postgresql://postgres:mysecretpassword@localhost:5432"

db = SQLAlchemy(app)

class GameModel(db.Model):
    __tablename__ = 'game'
    id = db.Column(db.Integer, primary_key=True)
    turnId = db.Column(db.Integer)
    x = db.Column(db.Integer)
    y = db.Column(db.Integer)
    playerid = db.Column(db.Integer)
    generated_at = db.Column(db.DateTime)


class PlayerModel(db.Model):
    playerid = db.Column(db.Integer, primary_key=True)
    username = db.Column(db.VARCHAR)
    botdir = db.Column(db.VARCHAR)
    updatedbot = db.Column(db.Bool)
    newbotdir = db.Column(db.VARCHAR)
    botstatus = db.Column(db.VARCHAR)

