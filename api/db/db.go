package db

import (
	"database/sql"
	"github.com/Salz-Team/salz/api/models"
)

type ApiDBHandler interface {
	GetDB() *sql.DB
	GetUser(int) (models.User, error)
	GetUserByLogin(string) (models.User, error)
	GetUserByIdentity(string, string) (models.User, error)
	CreateUser(models.User) (models.User, error)
	CreateBotFile(models.BotFile) (models.BotFile, error)
	ConfirmBotFile(models.BotFile) error
	GetMatches(sql.NullString, string, []int64, int64, int64) ([]models.Match, error)
}

type AuthDBHandler interface {
	GetDB() *sql.DB
	GetToken(string) (models.AuthToken, error)
	DeleteTokenByUserId(int) error
	DeleteTokenByToken(string) error
	CreateToken(models.AuthToken) error
	CreatePassword(int, string) error
	CheckPassword(int, string) (bool, error)
}
