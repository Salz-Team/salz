package controllers

import (
  "net/http"
  "strings"
  "github.com/gin-gonic/gin"
  "github.com/charmbracelet/log"
)

func (ctrl *Controller) GetUser(c *gin.Context) {
  // Get the user from the database
  
  authTokenHeader := c.GetHeader("Authorization")
  authToken := strings.TrimSpace(strings.Replace(authTokenHeader, "Bearer", "", 1))

  token, err := ctrl.cfg.AuthDBHandler.GetToken(authToken)
  if err != nil {
    log.Error("Unable to get token", "error", err)
    c.AbortWithStatus(http.StatusInternalServerError)
    return
  }

  user, err := ctrl.cfg.ApiDBHandler.GetUser(token.UserId)
  if err != nil {
    log.Error("Unable to get user by id even though token was valid", "error", err)
    c.AbortWithStatus(http.StatusUnauthorized)
    return
  }

  c.JSON(http.StatusOK, gin.H{
    "userid": user.Id,
    "username": user.UserName,
    "iconpath": user.IconPath,
  })
}

