//go:build integration
// +build integration

package test

import (
    "bytes"
	b64 "encoding/base64"
    "mime/multipart"
	"encoding/json"
    "strconv"
	"fmt"
	"io"
	"net/http"
	"net/http/httptest"
	"testing"
	"github.com/Salz-Team/salz/api/router"
	"github.com/Salz-Team/salz/api/models"
	"github.com/gin-gonic/gin"
    "maps"
)

func testSetup() *gin.Engine {
    // Correct router should be set up if ENV=itest
    r := router.SetupRouter()
	gin.SetMode(gin.TestMode)

	return r
}

func callEndpointOrFail(t *testing.T, r *gin.Engine, method string, url string, body io.Reader, headers map[string]string) (*httptest.ResponseRecorder) {
    req, err := http.NewRequest(method, url, body)
    if err != nil {
        t.Fatalf("Unable to create %s request for %s: %v", method, url, err)
    }

    for k,v := range headers {
        req.Header.Add(k, v)
    }

    w := httptest.NewRecorder()
    r.ServeHTTP(w, req)

    return w
}

func getEndpointResponseOrFail[T any](t *testing.T, r *gin.Engine, method string, url string, body io.Reader, headers map[string]string) (int, T) {
    w := callEndpointOrFail(t, r, method, url, body, headers)
    resp := w.Result()
    defer resp.Body.Close()

    var obj T
    err := json.NewDecoder(resp.Body).Decode(&obj)
    if err != nil {
        t.Fatalf("Unable to decode body: %v", err)
    }

    return w.Code, obj
}

func basicAuthLogin(r *gin.Engine) (models.AuthToken, error) {
	username := "testuser"
	password := "testpassword"

	w := httptest.NewRecorder()

	req, err := http.NewRequest("POST", "/login/basic", nil)
	if err != nil {
		return models.AuthToken{}, fmt.Errorf("Unable to create request: %v", err)
	}

    basicAuthToken := b64.StdEncoding.EncodeToString(fmt.Appendf([]byte{}, "%s:%s", username, password))
	req.Header.Set("Authorization", fmt.Sprintf("Basic %s", basicAuthToken))

	r.ServeHTTP(w, req)

	if w.Code != 200 {
		return models.AuthToken{}, fmt.Errorf("Unable to authenticate with code %d: %v", w.Code, w.Body)
	}

	authToken := models.AuthToken{}
	respBytes, err := io.ReadAll(w.Body)
	if err != nil {
		return models.AuthToken{}, fmt.Errorf("Unable to read the response body: %v", err)
	}

	if err = json.Unmarshal(respBytes, &authToken); err != nil {
		return models.AuthToken{}, fmt.Errorf("Unable to parse login response as authToken: %v", err)
	}

	return authToken, nil
}

func TestBasicLogin(t *testing.T) {
	r := testSetup()

	_, err := basicAuthLogin(r)
	if err != nil {
		t.Fatalf("Unable to perform basic auth: %v", err)
	}
}

func TestUsersMe(t *testing.T) {
	r := testSetup()
	authToken, err := basicAuthLogin(r)
	if err != nil {
		t.Fatalf("Unable to perform basic auth: %v", err)
	}
    authHeader := map[string]string{"Authorization": fmt.Sprintf("Bearer %s", authToken.Token)}

    t.Run("/users/me no auth 401", func (t *testing.T) {
        expected := 307 // redirects to login
        w := callEndpointOrFail(t, r, "GET", "/users/me", nil, nil)
        if w.Code != expected {
            t.Errorf("Expected %v, got %v", expected, w.Code)
        }
    })

    t.Run("/users/me valid auth 200", func (t *testing.T) {
        expected := 200
        w := callEndpointOrFail(t, r, "GET", "/users/me", nil, authHeader)
        req, err := http.NewRequest("GET", "/users/me", nil)
        if err != nil {
            t.Fatalf("Unable to create request GET /users/me")
        }
        r.ServeHTTP(w, req)
        if w.Code != expected {
            t.Errorf("Expected %v, got %v", expected, w.Code)
        }
    })
}

func TestListGames(t *testing.T) {
	r := testSetup()
    t.Run("/games returns properly", func (t *testing.T) {
        w := callEndpointOrFail(t, r, "GET", "/games", nil, nil)
        expected := 200
        var gameResponse models.ListGameResponse
        resp := w.Result()
        defer resp.Body.Close()

        err := json.NewDecoder(resp.Body).Decode(&gameResponse)
        if err != nil {
            t.Fatalf("Unable to read list of games: %v", err)
        }

        if w.Code != expected {
            t.Errorf("Expected %v, got %v", expected, w.Code)
        }

    })
}

func TestBotUpload(t *testing.T) {
	r := testSetup()
	authToken, err := basicAuthLogin(r)
	if err != nil {
		t.Fatalf("Unable to perform basic auth: %v", err)
	}
    authHeader := map[string]string{"Authorization": fmt.Sprintf("Bearer %s", authToken.Token)}

    // Get games -- we need a gameid to upload a bot to.
    _, gameResponse := getEndpointResponseOrFail[models.ListGameResponse](t, r, "GET", "/games", nil, nil)
    if len(gameResponse.Games) < 1 {
        t.Fatal("List game response returned no games")
    }
    gameId := gameResponse.Games[0].Id

    t.Run("/:gameid/bots/upload returns properly", func (t *testing.T) {
        expected := http.StatusCreated

        randomBytes := make([]byte, 1024) // stand-in for a bot to be uploaded
        var buf bytes.Buffer
        writer := multipart.NewWriter(&buf)
        part, err := writer.CreateFormFile("data", "bot.bin")
        if err != nil {
            t.Fatalf("Unable to create form file: %v", err)
        }
        _, err = part.Write(randomBytes)
        if err != nil {
            t.Fatalf("Unable to write bytes to form: %v", err)
        }
        writer.Close()

        headers := map[string]string{}
        maps.Copy(headers, authHeader)
        maps.Copy(headers, map[string]string{"Content-Type": writer.FormDataContentType()})

        code, botUploadResponse := getEndpointResponseOrFail[models.BotUploadResponse](t, r, "POST", fmt.Sprintf("/%s/bots/upload", strconv.Itoa(gameId)), &buf, headers)

        t.Logf("Bot upload response: %+v", botUploadResponse)

        if code != expected {
            t.Errorf("Expected %v, got %v", expected, code)
        }
        
    })
}
