package main

import (
	"context"
	"fmt"
	"github.com/gorilla/mux"
	"github.com/jackc/pgx/v4"
	"k8s.io/client-go/kubernetes"
	"net/http"
	"strconv"
	"strings"

	"log"

	"github.com/dgrijalva/jwt-go"
	// req "github.com/dgrijalva/jwt-go/request"
	// "github.com/mitchellh/mapstructure"
	"github.com/shaj13/go-guardian/auth"
	"github.com/shaj13/go-guardian/auth/strategies/basic"
	"github.com/shaj13/go-guardian/auth/strategies/bearer"
	"github.com/shaj13/go-guardian/store"
	"os"
	"time"
)

const (
	JWTSECRET string = "secret"
)

var (
	PGNAME        string
	PGUSER        string
	PGPASS        string
	PGHOST        string
	PGPORT        string
	authenticator auth.Authenticator
	cache         store.Cache
	conn          *pgx.Conn
	err           error
	k8sClient     *kubernetes.Clientset
)

func setEnvVars() {
	PGNAME = os.Getenv("PGNAME")
	PGUSER = os.Getenv("PGUSER")
	PGPASS = os.Getenv("PGPASS")
	PGPORT = os.Getenv("PGPORT")
	PGHOST = os.Getenv("PGHOST")
}

// TODO: Write to DB properly
func handleSignup(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/json")
	switch r.Method {
	case "POST":
		// signup user
		w.WriteHeader(http.StatusCreated)
	default:
		w.WriteHeader(http.StatusNotFound)
	}
}

func roothandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusOK)
	w.Write([]byte(`{"message": "ello mate"}`))
}

func testHandler(w http.ResponseWriter, r *http.Request) {

	w.Header().Set("Content-Type", "application/json")
	user, err := getUserFromAuthHeader(r.Header.Get("Authorization"))
	if err != nil {
		w.WriteHeader(http.StatusInternalServerError)
		return
	}
	deploy, err := getDeploymentByUserLabel(user, k8sClient)

	w.WriteHeader(http.StatusOK)
	w.Write([]byte(fmt.Sprintf(`{"message": "fuck %s"}`, deploy.GetName())))
}

func main() {
	fmt.Println("vim-go")

	setEnvVars()
	setupGoGuardian()

	k8sClient, err = getClientSet("outcluster")
	if err != nil {
		panic("fuck")
	}

	conn, err = pgx.Connect(
		context.Background(),
		fmt.Sprintf("host=%s port=%s dbname=%s user=%s password=%s",
			PGHOST,
			PGPORT,
			PGNAME,
			PGUSER,
			PGPASS,
		),
	)
	if err != nil {
		fmt.Printf("Couldn't connect: %v\n", err)
	}

	r := mux.NewRouter()
	r.HandleFunc("/", roothandler)
	r.HandleFunc("/signup", setupNewUser).Methods("POST")
	r.HandleFunc("/status", middleware(http.HandlerFunc(getStatus)))
	r.HandleFunc("/getDeploy", middleware(http.HandlerFunc(testHandler)))
	r.HandleFunc("/submit", middleware(http.HandlerFunc(submitHandler)))
	r.HandleFunc("/v1/auth/token", middleware(http.HandlerFunc(createToken))).Methods("GET")
	fmt.Println("serving on 8080")
	http.ListenAndServe(":8080", r)
}

func testUserStuff(w http.ResponseWriter, r *http.Request) {
	user, pass, ok := r.BasicAuth()
	if !ok {
		fmt.Println("fuck")
		return
	}
	fmt.Println("User:", user)
	fmt.Println("Pass:", pass)
}

func createToken(w http.ResponseWriter, r *http.Request) {
	user, _, ok := r.BasicAuth()
	if !ok {
		fmt.Println("bad bad not good")
	}

	token := jwt.NewWithClaims(jwt.SigningMethodHS256, jwt.MapClaims{
		"iss": "salz-api",
		"sub": user,
		"aud": "any",
		"exp": time.Now().Add(time.Minute * 5).Unix(),
	})
	jwtToken, _ := token.SignedString([]byte(JWTSECRET))
	w.Write([]byte(jwtToken))
}

func submitHandler(w http.ResponseWriter, r *http.Request) {

	user, err := getUserFromAuthHeader(r.Header.Get("Authorization"))
	if err != nil {
		fmt.Println("Error trying to get username:", err)
		w.WriteHeader(http.StatusInternalServerError)
		return
	}
	image := r.FormValue("image")
	if image == "" {
		fmt.Println("image not set in form")
		w.WriteHeader(http.StatusBadRequest)
		return
	}
	deploy, err := getDeploymentByUserLabel(user, k8sClient)
	if err == ErrNoMatchingDeployment {
		fmt.Println("No matching deployment!")
		createDeploymentError := createNewDeployment(user, image, k8sClient)
		if createDeploymentError != nil {
			fmt.Println("Error creating deployment: ", createDeploymentError)
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		w.WriteHeader(http.StatusAccepted)
		return
	}
	if err != nil {
		fmt.Println("Error getting deploy:", err)
		w.WriteHeader(http.StatusInternalServerError)
		return
	}

	if err := r.ParseForm(); err != nil {
		fmt.Println("Error parsing form:", err)
		w.WriteHeader(http.StatusBadRequest)
		return
	}

	updateErr := updateDeploymentByUserLabel(user, image, deploy, k8sClient)
	if updateErr != nil {
		fmt.Println("Update err: ", updateErr)
		w.WriteHeader(http.StatusInternalServerError)
		return
	}
}

func setupNewUser(w http.ResponseWriter, r *http.Request) {
	// TODO: fix bug where trying to insert existing user increments the id counter anyways
	user, _, ok := r.BasicAuth()
	if !ok {
		fmt.Println("Invalid user setup")
		return
	}
	_, err := conn.Exec(
		context.Background(),
		"insert into users (name) values ($1)",
		user,
	)
	if err != nil {
		fmt.Println("Error inserting into database")
		w.WriteHeader(http.StatusBadRequest)
	}
	w.WriteHeader(http.StatusCreated)
}

func getUserFromAuthHeader(header string) (string, error) {
	jwtToken := strings.Split(header, " ")
	if len(jwtToken) == 2 {
		token, err := jwt.Parse(jwtToken[1], func(token *jwt.Token) (interface{}, error) {
			if _, ok := token.Method.(*jwt.SigningMethodHMAC); !ok {
				return nil, fmt.Errorf("Unexpected signing method: %v", token.Header["alg"])
			}
			return []byte(JWTSECRET), nil
		})
		if err != nil {
			fmt.Println("Error parsing token: ", err)
			return "", err
		}
		claims := token.Claims.(jwt.MapClaims)
		return claims["sub"].(string), nil
	}
	return "", fmt.Errorf("Authorization header invalid format")
}

func getStatus(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/json")

	user, err := getUserFromAuthHeader(r.Header.Get("Authorization"))
	if err != nil {
		fmt.Println("Error getting user")
		w.WriteHeader(http.StatusForbidden)
		return
	}

	w.Write([]byte(fmt.Sprintf(`{"message": "ello %s from status"}`, user)))
	w.WriteHeader(http.StatusOK)
}

func setupGoGuardian() {
	authenticator = auth.New()
	cache = store.NewFIFO(context.Background(), time.Minute*10)

	basicStrategy := basic.New(validateUser, cache)
	tokenStrategy := bearer.New(verifyToken, cache)

	authenticator.EnableStrategy(basic.StrategyKey, basicStrategy)
	authenticator.EnableStrategy(bearer.CachedStrategyKey, tokenStrategy)
}

func validateUser(ctx context.Context, r *http.Request, userName, password string) (auth.Info, error) {
	// TODO: Actually authenticate??? fan out to oauth service?
	// Search for username in database

	var id int64
	var name string

	err := conn.QueryRow(ctx, "select id, name from users where name=$1", userName).Scan(&id, &name)
	if err != nil {
		fmt.Printf("user %s not in db: %v\n", userName, err)
		return nil, fmt.Errorf("Invalid credentials")
	}

	return auth.NewDefaultUser(name, strconv.FormatInt(id, 10), nil, nil), nil
}

func middleware(next http.Handler) http.HandlerFunc {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		log.Println("Executing Auth Middleware")
		user, err := authenticator.Authenticate(r)
		if err != nil {
			code := http.StatusUnauthorized
			http.Error(w, http.StatusText(code), code)
			return
		}
		log.Printf("User %s Authenticated\n", user.UserName())
		next.ServeHTTP(w, r)
	})
}

func verifyToken(ctx context.Context, r *http.Request, tokenString string) (auth.Info, error) {
	token, err := jwt.Parse(tokenString, func(token *jwt.Token) (interface{}, error) {
		if _, ok := token.Method.(*jwt.SigningMethodHMAC); !ok {
			return nil, fmt.Errorf("Unexpected signing method: %v", token.Header["alg"])
		}
		return []byte("secret"), nil
	})

	if err != nil {
		return nil, err
	}

	if claims, ok := token.Claims.(jwt.MapClaims); ok && token.Valid {
		user := auth.NewDefaultUser(claims["sub"].(string), "", nil, nil)
		return user, nil
	}

	return nil, fmt.Errorf("Invaled token")
}
