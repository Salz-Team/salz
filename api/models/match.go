package models

import (
	"time"
)

type MatchParticipant struct {
	UserId int     `json:"userId"`
	BotId  int     `json:"botId"`
	Score  float32 `json:"score"`
}

type Match struct {
	Id           int64              `json:"id"`
	CreatedAt    time.Time          `json:"created_at"`
	UpdatedAt    time.Time          `json:"updated_at"`
	Status       string             `json:"status"`
	Participants []MatchParticipant `json:"participants"`
}
