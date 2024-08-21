package controllers

import (
	"github.com/charmbracelet/log"
	"github.com/gin-gonic/gin"
	"net/http"
)

func (ctrl *Controller) GetUser(c *gin.Context) {
	userId := c.GetInt("userid")
	user, err := ctrl.cfg.ApiDBHandler.GetUser(userId)
	if err != nil {
		log.Error("Unable to get user by id even though token was valid", "error", err)
		c.AbortWithStatus(http.StatusUnauthorized)
		return
	}

	c.JSON(http.StatusOK, gin.H{
		"userid":   user.Id,
		"username": user.UserName,
		"iconpath": user.IconPath,
	})
}
