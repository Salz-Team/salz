package controllers

import (
	"github.com/Salz-Team/salz/api/models"
	"github.com/charmbracelet/log"
	"github.com/gin-gonic/gin"
	"net/http"
	"strconv"
	"strings"
)

func (ctrl *Controller) BotUploadHandler(c *gin.Context) {
	userId := c.GetInt("userid")
	gameId, err := strconv.Atoi(c.Param("gameid"))
	if err != nil {
		log.Error("Unable to read gameId", "gameId", c.Param("gameid"), "error", err)
		return
	}

	// Note that file is a multipart.File which implements io.Reader
	file, fileHeader, err := c.Request.FormFile("data")
	if err != nil {
		log.Error("Error getting file from form", "error", err)
		c.AbortWithStatus(http.StatusInternalServerError)
		return
	}

	// TODO filesize check, content type, encoding, etc

	// Get the extension of the file
	extension := fileHeader.Filename[strings.LastIndex(fileHeader.Filename, "."):]

	// Transaction might not be a good idea if the upload takes a while
	// Create a new file in the database, note that upload path is null until file is uploaded.
	bot, err := ctrl.cfg.ApiDBHandler.CreateBotFile(models.BotFile{
		GameId: gameId,
		UserId: userId,
	})

	bot.UploadPath = bot.BotPathBuilder(extension)

	// Upload the file to S3
	err = ctrl.cfg.ObjectStoreHandler.UploadBotFile(bot.UploadPath, file)
	if err != nil {
		log.Error("Error uploading bot file", "error", err)
		c.AbortWithStatus(http.StatusInternalServerError)
		return
	}

	// Update the file in the database
	err = ctrl.cfg.ApiDBHandler.ConfirmBotFile(bot)
	if err != nil {
		log.Error("Error confirming bot file", "error", err)
		c.AbortWithStatus(http.StatusInternalServerError)
		return
	}

	c.JSON(http.StatusCreated, models.BotUploadResponse{GameId: bot.GameId, BotId: bot.BotId})
}
