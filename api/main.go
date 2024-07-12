package main

import (
	"github.com/Salz-Team/salz/api/config"
	"github.com/Salz-Team/salz/api/controllers"
	"github.com/Salz-Team/salz/api/middlewares"
	"github.com/gin-gonic/gin"
)

func main() {
	cfg := config.NewConfig()
	ctrl := controllers.NewController(cfg)

	r := gin.Default()

	// All routes should start with /api

	// Public routes
	r.GET("/health", controllers.Ping)
	r.GET("/login", ctrl.OAuthLoginHandler)
	r.GET("/login/callback", ctrl.OAuthCallbackHandler)

	// Protected routes
	protectedRoutes := r.Group("")
	protectedRoutes.Use(middlewares.AuthMiddleware(cfg.AuthDBHandler))
	{
		protectedRoutes.GET("/user/me", ctrl.GetUser)
		protectedRoutes.POST("/bot/upload", ctrl.BotUploadHandler)
	}

	r.Run()
}
