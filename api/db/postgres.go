package db

import (
  "database/sql" 
  "github.com/Salz-Team/salz/api/models"
  _ "github.com/glebarez/go-sqlite"
  "github.com/charmbracelet/log"
  _ "github.com/lib/pq"
  "os"
  "errors"
)

// Specific error type for when token is not found
var ErrTokenNotFound = errors.New("Token not found")

// PostgresHandler implements both ApiDBHandler and AuthDBHandler
type PostgresHandler struct {
  DB *sql.DB
}

func NewPostgresHandler() *PostgresHandler {
  pgUri := os.Getenv("PG_URI")
  if pgUri == "" {
    log.Fatal("PG_URI environment variable not set")
  }

  db, err := sql.Open("postgres", pgUri)
  if err != nil {
    log.Fatal("Unable to open Postgres database", "error", err)
  }
  return &PostgresHandler{DB: db}
}

func (p *PostgresHandler) GetDB() *sql.DB {
  return p.DB
}

func (p *PostgresHandler) GetUser(id int) (models.User, error) {
  user := models.User{}
  row := p.DB.QueryRow("SELECT * FROM salz.users WHERE id = $1", id)
  err := row.Scan(&user.Id, &user.UserName, &user.IconPath, &user.IdentityProvider, &user.IdentityProviderId, &user.Elo)
  if err != nil {
    log.Error("Unable to get user by id", "error", err)
    return models.User{}, err
  }
  return user, nil
}

func (p *PostgresHandler) GetUserByLogin (username string) (models.User, error) {
  user := models.User{}
  row := p.DB.QueryRow("SELECT * FROM salz.users WHERE username = $1", username)
  err := row.Scan(&user.Id, &user.UserName, &user.IconPath, &user.IdentityProvider, &user.IdentityProviderId, &user.Elo)
  if err != nil {
    log.Error("Unable to get user by username", "error", err)
    return models.User{}, err
  }
  return user, nil
}

// Identity here refers to the ID given by the identity provider used by the user to authenticate.
// This is not necessarily the username. For example, while Github does have "usernames", we will instead use the
// static ID that Github assigns to each user (since usernames can be changed).
func (p *PostgresHandler) GetUserByIdentity (identityProvider, identityProviderId string) (models.User, error) {
  user := models.User{}
  row := p.DB.QueryRow("SELECT * FROM salz.users WHERE identity_provider = $1 and identity_provider_id = $2", identityProvider, identityProviderId)
  err := row.Scan(&user.Id, &user.UserName, &user.IconPath, &user.IdentityProvider, &user.IdentityProviderId, &user.Elo)
  if err != nil {
    log.Error("Unable to get user by idp", "error", err)
    return models.User{}, err
  }
  return user, nil
}

func (p *PostgresHandler) CreateUser (user models.User) (models.User, error) {
  _, err := p.DB.Exec("INSERT INTO salz.users (username, icon_path, identity_provider, identity_provider_id, elo) VALUES ($1, $2, $3, $4, $5)", user.UserName, user.IconPath, user.IdentityProvider, user.IdentityProviderId, user.Elo)
  if err != nil {
    log.Error("Unable to create user", "error", err)
    return models.User{}, err
  }
  // Maybe we should just do a ... returning Id? idk
  return p.GetUserByLogin(user.UserName)
}

func (p *PostgresHandler) GetToken (token string) (models.AuthToken, error) {
  authToken := models.AuthToken{}
  row := p.DB.QueryRow("SELECT user_id, token, expires_at FROM auth.sessions WHERE token = $1", token)
  err := row.Scan(&authToken.UserId, &authToken.Token, &authToken.ExpiresAt)
  if err != nil {
    log.Error("Unable to get token", "error", err)
    return models.AuthToken{}, err
  }
  return authToken, nil
}

func (p *PostgresHandler) DeleteTokenByToken (token string) error {
  _, err := p.DB.Exec("DELETE FROM auth.sessions WHERE token = $1", token)
  if err != nil {
    log.Error("Unable to delete token", "error", err)
    return err
  }
  return nil
}

func (p *PostgresHandler) DeleteTokenByUserId (userId int) error {
  _, err := p.DB.Exec("DELETE FROM auth.sessions WHERE user_id = $1", userId)
  if err != nil {
    log.Error("Unable to delete token by user id", "error", err)
    return err
  }
  return nil
}

func (p *PostgresHandler) CreateToken (authToken models.AuthToken) error {
  _, err := p.DB.Exec("INSERT INTO auth.sessions (user_id, token, expires_at) VALUES ($1, $2, $3)", authToken.UserId, authToken.Token, authToken.ExpiresAt)
  if err != nil {
    log.Error("Unable to create token", "error", err)
    return err
  }
  return nil
}
