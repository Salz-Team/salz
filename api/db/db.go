package db

import (
	"database/sql"
	"github.com/Salz-Team/salz/api/models"
)

type ApiDBHandler interface {
	GetDB() *sql.DB
	GetUser(id int) (models.User, error)
	GetUserByLogin(username string) (models.User, error)
	GetUserByIdentity(identityProvider string, identityProviderId string) (models.User, error)
	CreateUser(user models.User) (models.User, error)
	CreateBotFile(botFile models.BotFile) (models.BotFile, error)
	ConfirmBotFile(botFile models.BotFile) error
	ListGames(limit int, offset int) ([]models.Game, bool, error)
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
