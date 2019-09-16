case "${FRONTEND_ENV}" in
  "docker-prod")
    npm run build
    npm start
    ;;
  "docker-dev")
    npm run dev:docker
    ;;
  "dev"|*)
    npm run dev
    ;;
esac
