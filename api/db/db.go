package db

import (
  "database/sql" 
  "github.com/Salz-Team/salz/api/models"
  _ "github.com/glebarez/go-sqlite"
  "github.com/charmbracelet/log"
)

type ApiDBHandler interface {
  GetDB() *sql.DB
  GetUser(int) (models.User, error)
  GetUserByLogin(string) (models.User, error)
  CreateUser(models.User) (models.User, error)
}

type AuthDBHandler interface {
  GetDB() *sql.DB
  GetToken(string) (models.AuthToken, error)
  DeleteToken(string) error
  CreateToken(models.AuthToken) error
}

type SQliteHandler struct {
  DB *sql.DB
}

func NewSqliteHandler() *SQliteHandler {
  db, err := sql.Open("sqlite", "./salz.db")
  if err != nil {
    log.Fatal("Unable to open SQLite database", "error", err)
  }

  userTableDDL := `CREATE TABLE IF NOT EXISTS users (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    username TEXT NOT NULL,
    iconpath TEXT NOT NULL,
    identityprovider TEXT NOT NULL
  );`

  authTokenTableDDL := `CREATE TABLE IF NOT EXISTS auth_tokens (
    user_id INTEGER PRIMARY KEY NOT NULL,
    token TEXT NOT NULL,
    expires_at DATETIME NOT NULL
  );`

  _, err = db.Exec(userTableDDL)
  if err != nil {
    log.Fatal("Unable to create users table", "error", err)
  }

  _, err = db.Exec(authTokenTableDDL)
  if err != nil {
    log.Fatal("Unable to create auth_tokens table", "error", err)
  }

  return &SQliteHandler{DB: db}
}

func (s *SQliteHandler) GetDB() *sql.DB {
  return s.DB
}

func (s *SQliteHandler) GetUser(id int) (models.User, error) {
  user := models.User{}
  row := s.DB.QueryRow("SELECT * FROM users WHERE id = ?", id)
  err := row.Scan(&user.Id, &user.UserName, &user.IconPath, &user.IdentityProvider)
  if err != nil {
    log.Error("Unable to get user", "error", err)
    return models.User{}, err
  }
  return user, nil
}

func (s *SQliteHandler) GetUserByLogin(login string) (models.User, error) {
  user := models.User{}
  row := s.DB.QueryRow("SELECT * FROM users WHERE username = ?", login)
  err := row.Scan(&user.Id, &user.UserName, &user.IconPath, &user.IdentityProvider)
  if err != nil {
    log.Error("Unable to get user by login", "error", err)
    return models.User{}, err
  }
  return user, nil
}

func (s *SQliteHandler) CreateUser(user models.User) (models.User, error) {
  res, err := s.DB.Exec("INSERT INTO users (username, iconpath, identityprovider) VALUES (?, ?, ?)",
    user.UserName, user.IconPath, user.IdentityProvider)
  if err != nil {
    log.Error("Unable to create user", "error", err)
    return models.User{}, err
  }
  id, err := res.LastInsertId()
  if err != nil {
    log.Error("Unable to get last insert id", "error", err)
    return models.User{}, err
  }
  user.Id = int(id)
  return user, nil
}

func (s *SQliteHandler) GetToken(token string) (models.AuthToken, error) {
  authToken := models.AuthToken{}
  row := s.DB.QueryRow("SELECT * FROM auth_tokens WHERE token = ?", token)
  err := row.Scan(&authToken.UserId, &authToken.Token, &authToken.ExpiresAt)
  if err != nil {
    log.Error("Unable to get token", "error", err)
    return models.AuthToken{}, err
  }
  return authToken, nil
}

func (s *SQliteHandler) DeleteToken(token string) error {
  _, err := s.DB.Exec("DELETE FROM auth_tokens WHERE token = ?", token)
  if err != nil {
    log.Error("Unable to delete token", "error", err)
    return err
  }
  return nil
}

func (s *SQliteHandler) CreateToken(authToken models.AuthToken) error {
  _, err := s.DB.Exec("INSERT OR REPLACE INTO auth_tokens (user_id, token, expires_at) VALUES (?, ?, ?)",
    authToken.UserId, authToken.Token, authToken.ExpiresAt)
  if err != nil {
    log.Error("Unable to create token", "error", err)
    return err
  }
  return nil
}
