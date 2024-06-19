package models

// Steal the name, icon from Github idp
type User struct {
  Id int `json:"id"`
  UserName string `json:"username"`
  IconPath string `json:"iconpath"`
  IdentityProvider string `json:"identityprovider"`
}
