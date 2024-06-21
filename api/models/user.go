package models

// Steal the name, icon from Github idp
type User struct {
  Id int `json:"id"`
  UserName string `json:"username"`
  IconPath string `json:"iconpath"`
  IdentityProvider string `json:"identityprovider"`
  IdentityProviderId string `json:"identityproviderid"`
  Elo float32 `json:"elo"`
}
