package controllers

import (
	"database/sql"
	"github.com/charmbracelet/log"
	"github.com/gin-gonic/gin"
	"net/http"

	"strconv"
	"strings"
)

func (ctrl *Controller) GetMatches(c *gin.Context) {
	// Filter and sort parameters
	// Default: filter completed, sort by completed at descending

	allowedStatusFilters := map[string]bool{
		"Finished": true,
		"Crashed":  true,
		"Running":  true,
		"Pending":  true,
		"All":      true,
	}

	allowedSorts := map[string]bool{
		"updatedAtAsc":  true,
		"updatedAtDesc": true,
		"createdAtAsc":  true,
		"createdAtDesc": true,
	}

	filterBy := c.DefaultQuery("filterStatusBy", "Finished")
	sortBy := c.DefaultQuery("sortBy", "updatedAtDesc")
	matchIdsStr := strings.Split(c.DefaultQuery("matchIds", ""), ",")
	limitStr := c.DefaultQuery("limit", "1000")
	offsetStr := c.DefaultQuery("offset", "0")

	// Validate filterBy and sortBy
	if _, ok := allowedStatusFilters[filterBy]; !ok {
		log.Error("Invalid filterBy parameter", "filterBy", filterBy)
		c.AbortWithStatus(http.StatusBadRequest)
		return
	}
	var filterByParam sql.NullString
	if filterBy == "All" {
		filterByParam = sql.NullString{Valid: false}
	} else {
		filterByParam = sql.NullString{Valid: true, String: filterBy}
	}

	if _, ok := allowedSorts[sortBy]; !ok {
		log.Error("Invalid sortBy parameter", "sortBy", sortBy)
		c.AbortWithStatus(http.StatusBadRequest)
		return
	}

	// Convert matchIdsParam to int64
	var matchIdsParam []int64
	if len(matchIdsStr) == 1 && matchIdsStr[0] == "" {
		matchIdsParam = []int64{}
	} else {
		matchIdsParam = make([]int64, len(matchIdsStr))
		for _, matchIdStr := range matchIdsStr {
			matchId, err := strconv.ParseInt(matchIdStr, 10, 64)
			if err != nil {
				log.Error("Unable to parse matchIds", "matchIds", matchIdsStr)
				c.AbortWithStatus(http.StatusBadRequest)
				return
			}
			matchIdsParam = append(matchIdsParam, matchId)
		}
	}

	// Convert limitParam and offset to int64
	limitParam, err := strconv.ParseInt(limitStr, 10, 64)
	if err != nil {
		log.Error("Unable to parse limit", "limit", limitStr)
		c.AbortWithStatus(http.StatusBadRequest)
		return
	}

	offsetParam, err := strconv.ParseInt(offsetStr, 10, 64)
	if err != nil {
		log.Error("Unable to parse offset", "offset", offsetStr)
		c.AbortWithStatus(http.StatusBadRequest)
		return
	}

	matches, err := ctrl.cfg.ApiDBHandler.GetMatches(
		filterByParam,
		sortBy,
		matchIdsParam,
		offsetParam,
		limitParam,
	)
	if err != nil {
		log.Error("Unable to get matches", "filterBy", filterBy, "sortBy", sortBy)
		c.AbortWithStatus(http.StatusInternalServerError)
		return
	}

	c.JSON(http.StatusOK, matches)
}

func (ctrl *Controller) GetMatch(c *gin.Context) {
	matchIdPath := c.Param("matchid")
	matchId, err := strconv.ParseInt(matchIdPath, 10, 64)
	if err != nil {
		log.Error("Unable to parse matchid from path", "matchid", matchIdPath)
		c.AbortWithStatus(http.StatusBadRequest)
		return
	}

	match, err := ctrl.cfg.ApiDBHandler.GetMatches(
		sql.NullString{},
		"updatedAtDesc",
		[]int64{matchId},
		0,
		1,
	)
	if err != nil {
		log.Error("Unable to get match", "matchid", matchId)
		c.AbortWithStatus(http.StatusNotFound)
		return
	}

	c.JSON(http.StatusOK, match)
}
