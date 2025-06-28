package main

import (
	"github.com/Salz-Team/salz/api/router"
)

func main() {
	r := router.SetupRouter()
	r.Run()
}
