package router

import (
	"github.com/Salz-Team/salz/api/config"
	"github.com/Salz-Team/salz/api/controllers"
	"github.com/Salz-Team/salz/api/middlewares"
	"github.com/gin-gonic/gin"
)

func SetupRouter() *gin.Engine {
	cfg := config.NewConfig()
	ctrl := controllers.NewController(cfg)

	r := gin.New()
	for _, hf := range cfg.GinMiddlewares {
		r.Use(hf)
	}

	gin.SetMode(cfg.GinReleaseMode)
	r.SetTrustedProxies(nil)

	// All routes should start with /api

	// Public routes
	r.GET("/health", controllers.Ping)
	r.POST("/login/basic", ctrl.BasicAuthLoginHandler)
	r.GET("/login", ctrl.OAuthLoginHandler)
	r.GET("/login/callback", ctrl.OAuthCallbackHandler)
	r.GET("/games", ctrl.ListGames)

	// Protected routes
	protectedRoutes := r.Group("")
	protectedRoutes.Use(middlewares.AuthMiddleware(cfg.AuthDBHandler))
	{
		protectedRoutes.GET("/users/me", ctrl.GetUser)
		protectedRoutes.POST("/:gameid/bots/upload", ctrl.BotUploadHandler)
	}

	return r
}
