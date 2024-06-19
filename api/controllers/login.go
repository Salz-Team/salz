package controllers

import (
  "net/http"
  "github.com/gin-gonic/gin"
  "github.com/charmbracelet/log"
  "github.com/Salz-Team/salz/api/models"

  ghlib "github.com/google/go-github/v62/github"
)

func (ctrl *Controller) OAuthLoginHandler(c *gin.Context) {
  url := ctrl.cfg.OAuth2Config.AuthCodeURL("ain't got no state yet")
  log.Info("Login redirect", "url", url)
  c.Redirect(http.StatusTemporaryRedirect, url)
}

func (ctrl *Controller) OAuthCallbackHandler(c *gin.Context) {
  // TODO Check the state to prevent CSRF attacks
  code := c.Query("code")
  token, err := ctrl.cfg.OAuth2Config.Exchange(c, code)
  if err != nil {
    log.Error("oauth2: Exchange failed", "error", err)
    c.AbortWithStatus(http.StatusUnauthorized)
    return
  }

  // Instead of pulling in this massive library, we could have done a basic query against the API.
  // But I didn't want to set up the models.
  ghc := ghlib.NewClient(nil).WithAuthToken(token.AccessToken)
  user, _, err := ghc.Users.Get(c, "")
  if err != nil {
    log.Error("Unable to get GH user info", "error", err)
    c.AbortWithStatus(http.StatusInternalServerError)
    return
  }

  // TODO: use the static github user ID instead of the username since that can change.
  // We also need to associate the static ID along with the identity provider.

  // Check the db for the user. If it doesn't exist, create it.
  u, err := ctrl.cfg.ApiDBHandler.GetUserByLogin(*user.Login)
  if err != nil {
    log.Warn("Unable to get user by login", "error", err)
    // Create the user
    u = models.User{
      UserName: *user.Login,
      IconPath: *user.AvatarURL,
      IdentityProvider: "github",
    }
    u, err = ctrl.cfg.ApiDBHandler.CreateUser(u)
    if err != nil {
      log.Error("Unable to create user", "error", err)
      c.AbortWithStatus(http.StatusInternalServerError)
      return
    }
    log.Info("User created", "user", u)
  }

  // Create a token for the user
  userAuthToken := models.NewAuthTokenForUser(u.Id)
  err = ctrl.cfg.AuthDBHandler.CreateToken(*userAuthToken)
  if err != nil {
    log.Error("Unable to create token", "error", err)
    c.AbortWithStatus(http.StatusInternalServerError)
    return
  }

  c.JSON(http.StatusOK, gin.H{
    "access_token": userAuthToken.Token,
    "expires_at": userAuthToken.ExpiresAt,
  })
}
