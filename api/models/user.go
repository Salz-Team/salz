package models

import (
	"github.com/guregu/null/v6"
	"time"
)

// Steal the name, icon from Github idp
type User struct {
	Id                 int         `json:"id"`
	UserName           string      `json:"username"`
	CreatedAt          time.Time   `json:"created_at"`
	UpdatedAt          time.Time   `json:"updated_at"`
	IconPath           null.String `json:"iconpath"`
	IdentityProvider   string      `json:"identityprovider"`
	IdentityProviderId string      `json:"identityproviderid"`
	Elo                float32     `json:"elo"`
}
