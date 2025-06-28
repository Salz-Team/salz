package controllers

import (
	"github.com/Salz-Team/salz/api/models"
	"github.com/charmbracelet/log"
	"github.com/gin-gonic/gin"
	"net/http"
	"strconv"
)

func (ctrl *Controller) ListGames(c *gin.Context) {
	limit, err := strconv.Atoi(c.DefaultQuery("limit", "1000"))
	if err != nil {
		log.Error("Unable to parse limit", "error", err)
		c.AbortWithStatus(http.StatusBadRequest)
	}
	offset, err := strconv.Atoi(c.DefaultQuery("offset", "0"))
	if err != nil {
		log.Error("Unable to parse offset", "error", err)
		c.AbortWithStatus(http.StatusBadRequest)
	}
	games, hasMore, err := ctrl.cfg.ApiDBHandler.ListGames(limit, offset)
	if err != nil {
		log.Error("Unable to list games", "error", err)
		c.AbortWithStatus(http.StatusInternalServerError)
		return
	}

	c.JSON(http.StatusOK, models.ListGameResponse{Games: games, HasMore: hasMore})
}
