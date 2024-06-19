package middlewares

import (
  "github.com/gin-gonic/gin"
  "github.com/charmbracelet/log"
  "net/http"
  "strings"
  "github.com/Salz-Team/salz/api/db"
  "time"
  "database/sql"
)

func AuthMiddleware(authDb db.AuthDBHandler) gin.HandlerFunc {
    return func(c *gin.Context) {
        log.Debug("Auth middleware")
        authTokenHeader := c.GetHeader("Authorization")

        if authTokenHeader == "" {
            log.Info("No token found")
            c.Redirect(http.StatusTemporaryRedirect, "/login")
            c.Abort()
            return
        }

        authToken := strings.TrimSpace(strings.Replace(authTokenHeader, "Bearer", "", 1))

        // Check if the token is valid
        token, err := authDb.GetToken(authToken)
        if err == sql.ErrNoRows {
            log.Debug("Token not found")
            c.Redirect(http.StatusTemporaryRedirect, "/login")
            c.Abort()
            return
        } else if err != nil {
            log.Error("Unable to get token", "error", err)
            c.AbortWithStatus(http.StatusInternalServerError)
            return
        }

        if token.ExpiresAt < time.Now().Unix() {
            log.Debug("Token expired")
            c.Redirect(http.StatusTemporaryRedirect, "/login")
            c.Abort()
            return
        }

        log.Debug("Token for user is valid", "userid", token.UserId)

        c.Next()
    }
}
