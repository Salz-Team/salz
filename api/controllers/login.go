package controllers

import (
	"encoding/json"
	"fmt"
	"net/http"
	"net/url"
	"strconv"

	"github.com/Salz-Team/salz/api/models"
	"github.com/charmbracelet/log"
	"github.com/gin-gonic/gin"
	"database/sql"

	ghlib "github.com/google/go-github/v62/github"
)

// TODO find a good place for this
const (
	DEFAULT_ELO = 1000.0
)

// Creates user if the user doesn't exist. Otherwise, authenticates against provided password.
// Returns a token if the user is authenticated or was just created.
func (ctrl *Controller) BasicAuthLoginHandler(c *gin.Context) {
	user, pass, ok := c.Request.BasicAuth()
	if !ok {
		log.Warn("No basic auth header provided")
		c.AbortWithStatus(http.StatusUnauthorized)
		return
	}

	if !ctrl.cfg.EnableBasicAuth {
		log.Warn("Basic auth is disabled")
		c.AbortWithStatus(http.StatusUnauthorized)
		return
	}

	// Check if the user already exists
	salzUser, userLookupErr := ctrl.cfg.ApiDBHandler.GetUserByLogin(user)
	var authenticated bool

	if userLookupErr == nil {
		var err error
		authenticated, err = ctrl.cfg.AuthDBHandler.CheckPassword(salzUser.Id, pass)
		if err != nil {
			log.Error("Unable to check password", "error", err)
			c.AbortWithStatus(http.StatusInternalServerError)
			return
		}
	} else if userLookupErr == sql.ErrNoRows {
		var err error
		// Create the user
		u := models.User{
			UserName: user,
			Elo:      DEFAULT_ELO,
			IdentityProvider: "basicauth",
			IdentityProviderId: user,
		}
		salzUser, err = ctrl.cfg.ApiDBHandler.CreateUser(u)
		if err != nil {
			log.Error("Unable to create user", "error", err)
			c.AbortWithStatus(http.StatusInternalServerError)
			return
		}

		// Create the password
		err = ctrl.cfg.AuthDBHandler.CreatePassword(salzUser.Id, pass)
		if err != nil {
			log.Error("Unable to create password", "error", err)
			c.AbortWithStatus(http.StatusInternalServerError)
			return
		}

		authenticated = true
	} else {
		log.Error("Unable to get user by login", "error", userLookupErr)
		c.AbortWithStatus(http.StatusInternalServerError)
		return
	}

	if authenticated {
		// Create a token for the user
		userAuthToken := models.NewAuthTokenForUser(salzUser.Id, ctrl.cfg.AuthTokenValidDuration)
		err := ctrl.cfg.AuthDBHandler.CreateToken(*userAuthToken)
		if err != nil {
			log.Error("Unable to create token", "error", err)
			c.AbortWithStatus(http.StatusInternalServerError)
			return
		}
		c.JSON(http.StatusOK, gin.H{
			"access_token": userAuthToken.Token,
			"expires_at":   userAuthToken.ExpiresAt,
		})
		return
	}
	log.Warn("Authentication failed", "user", user)
	c.AbortWithStatus(http.StatusUnauthorized)
	return
}

type State struct {
	Code        string `json:"code"`
	RedirectUri string `json:"redirect_uri"`
}

func (ctrl *Controller) OAuthLoginHandler(c *gin.Context) {
	state := State{
		Code:        "ain't got no state yet",
		RedirectUri: c.Query("redirect_uri"),
	}
	b, err := json.Marshal(state)
	if err != nil {
		log.Error("Could not create state for login", "error", err)
		c.AbortWithStatus(http.StatusInternalServerError)
		return
	}
	authUrl := ctrl.cfg.OAuth2Config.AuthCodeURL(string(b[:]))
	log.Info("Login redirect", "url", authUrl)
	c.Redirect(http.StatusTemporaryRedirect, authUrl)
}

func (ctrl *Controller) OAuthCallbackHandler(c *gin.Context) {
	// TODO Maybe redo encodedState message to do checks to prevent CSRF attacks?
	encodedState := c.Query("state")
	var state State
	if err := json.Unmarshal([]byte(encodedState), &state); err != nil {
		log.Error("Could not parse state", "error", err)
		c.AbortWithStatus(http.StatusInternalServerError)
		return
	}

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

	// Check the db for the user. If it doesn't exist, create it.
	userId := strconv.FormatInt(*user.ID, 10)
	idp := "github"
	u, err := ctrl.cfg.ApiDBHandler.GetUserByIdentity(idp, userId)
	if err != nil {
		log.Warn("Unable to get user by identity", "error", err)
		// Create the user
		u = models.User{
			UserName:           *user.Login,
			IconPath:           *user.AvatarURL,
			IdentityProvider:   idp,
			IdentityProviderId: userId,
			Elo:                DEFAULT_ELO,
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
	userAuthToken := models.NewAuthTokenForUser(u.Id, ctrl.cfg.AuthTokenValidDuration)
	err = ctrl.cfg.AuthDBHandler.CreateToken(*userAuthToken)
	if err != nil {
		log.Error("Unable to create token", "error", err)
		c.AbortWithStatus(http.StatusInternalServerError)
		return
	}

	c.Header("Set-Cookie", fmt.Sprintf("Access-Token=%s; Expires=%s; HttpOnly; Secure; SameSite=Lax; Path=/", userAuthToken.Token, userAuthToken.ExpiresAt.Format(http.TimeFormat)))

	// If the redirect uri is empty, send the access token back to the client
	if state.RedirectUri == "" {
		c.JSON(http.StatusOK, gin.H{
			"access_token": userAuthToken.Token,
			"expires_at":   userAuthToken.ExpiresAt,
		})
		return
	}

	webUrl, err := url.JoinPath(ctrl.cfg.WebBaseUrl, "/login/callback")
	if err != nil {
		log.Error("Unable to generate web redirect url", "error", err)
		c.AbortWithStatus(http.StatusInternalServerError)
		return
	}

	redirectUrl, err := http.NewRequest("GET", webUrl, nil)
	if err != nil {
		log.Error("Could not create redirect url to web", "error", err)
		c.AbortWithStatus(http.StatusInternalServerError)
		return
	}

	redirectUrl.URL.RawQuery = url.Values{
		"redirect_uri": {state.RedirectUri},
	}.Encode()

	c.Redirect(http.StatusTemporaryRedirect, redirectUrl.URL.String())
}
