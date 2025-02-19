package controllers

import (
	"database/sql"
	"github.com/charmbracelet/log"
	"github.com/gin-gonic/gin"
	"net/http"

	"strconv"

	"github.com/Salz-Team/salz/api/models"
)

type MatchesRequest struct {
	FilterStatusBy string  `form:"filterStatusBy"`
	SortBy         string  `form:"sortBy"`
	MatchIds       []int64 `form:"matchIds"`
	Limit          int64   `form:"limit"`
	Offset         int64   `form:"offset"`
}

type MatchesResponse struct {
	Matches []models.Match `json:"matches"`
	HasMore bool           `json:"hasMore"`
}

func (ctrl *Controller) GetMatches(c *gin.Context) {
	// Filter and sort parameters
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

	// Default: filter completed, sort by completed at descending
	matchesRequest := MatchesRequest{
		FilterStatusBy: "Finished",
		SortBy:         "updatedAtDesc",
		MatchIds:       []int64{},
		Limit:          1000,
		Offset:         0,
	}
	err := c.Bind(&matchesRequest)
	if err != nil {
		log.Error("Unable to bind request", "error", err)
		c.AbortWithStatus(http.StatusBadRequest)
		return
	}

	log.Debug("MatchesRequest", "matchesRequest", matchesRequest)

	// Validate filterBy and sortBy
	if _, ok := allowedStatusFilters[matchesRequest.FilterStatusBy]; !ok {
		log.Error("Invalid filterBy parameter", "filterBy", matchesRequest.FilterStatusBy)
		c.AbortWithStatus(http.StatusBadRequest)
		return
	}
	var filterBy sql.NullString
	// "All" is represented in the db query as a null filter.
	if matchesRequest.FilterStatusBy == "All" {
		filterBy = sql.NullString{Valid: false}
	} else {
		filterBy = sql.NullString{Valid: true, String: matchesRequest.FilterStatusBy}
	}

	if _, ok := allowedSorts[matchesRequest.SortBy]; !ok {
		log.Error("Invalid sortBy parameter", "sortBy", matchesRequest.SortBy)
		c.AbortWithStatus(http.StatusBadRequest)
		return
	}

	matches, hasMore, err := ctrl.cfg.ApiDBHandler.GetMatches(
		filterBy,
		matchesRequest.SortBy,
		matchesRequest.MatchIds,
		matchesRequest.Offset,
		matchesRequest.Limit,
	)
	if err != nil {
		log.Error("Unable to get matches", "matchesRequest", matchesRequest)
		c.AbortWithStatus(http.StatusInternalServerError)
		return
	}

	c.JSON(http.StatusOK, MatchesResponse{Matches: matches, HasMore: hasMore})
}

func (ctrl *Controller) GetMatch(c *gin.Context) {
	matchIdPath := c.Param("matchid")
	matchId, err := strconv.ParseInt(matchIdPath, 10, 64)
	if err != nil {
		log.Error("Unable to parse matchid from path", "matchid", matchIdPath)
		c.AbortWithStatus(http.StatusBadRequest)
		return
	}

	match, _, err := ctrl.cfg.ApiDBHandler.GetMatches(
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

    if len(match) == 0 {
        log.Error("Could not find match")
        c.AbortWithStatus(http.StatusNotFound)
        return
    }

	c.JSON(http.StatusOK, match[0])
}
