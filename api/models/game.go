package models

import (
	"github.com/guregu/null/v6"
	"time"
)

type ListGameResponse struct {
	Games   []Game `json:"games"`
	HasMore bool   `json:"hasMore"`
}

type Game struct {
	Id        int         `json:"id"`
	Name      string      `json:"name"`
	CreatedAt time.Time   `json:"created_at"`
	UpdatedAt time.Time   `json:"updated_at"`
	CreatedBy int         `json:"created_by"`
	IconPath  null.String `json:"iconpath"`
}
