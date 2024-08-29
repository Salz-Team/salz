package middlewares

import (
	"database/sql"
	"github.com/Salz-Team/salz/api/db"
	"github.com/charmbracelet/log"
	"github.com/gin-gonic/gin"
	"net/http"
	"strings"
	"time"
)

func AuthMiddleware(authDb db.AuthDBHandler) gin.HandlerFunc {
	return func(c *gin.Context) {
		log.Debug("Auth middleware")
		authTokenHeader := c.GetHeader("Authorization")

		if authTokenHeader == "" {
			log.Info("No token found")
			c.Redirect(http.StatusTemporaryRedirect, "/login")
			return
		}

		authToken := strings.TrimSpace(strings.Replace(authTokenHeader, "Bearer", "", 1))

		token, err := authDb.GetToken(authToken)
		if err == sql.ErrNoRows {
			log.Debug("Token not found (err no rows)")
			c.AbortWithStatus(http.StatusUnauthorized)
			return
		} else if err == db.ErrTokenNotFound {
			log.Debug("Token not found (err token not found)")
			c.AbortWithStatus(http.StatusUnauthorized)
			return
		} else if err != nil {
			log.Error("Unable to get token", "error", err)
			c.AbortWithStatus(http.StatusInternalServerError)
			return
		}

		// Is this the right behaviour?
		if token.ExpiresAt.Before(time.Now()) {
			log.Debug("Token expired")
			c.Redirect(http.StatusTemporaryRedirect, "/login")
			return
		}

		log.Debug("Token for user is valid", "userid", token.UserId)

		// Set the user id in the context
		c.Set("userid", token.UserId)

		c.Next()
	}
}
