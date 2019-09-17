FROM node

COPY . /app
WORKDIR /app

# uses these env vars, can override the conf
ENV NUXT_HOST=0.0.0.0
ENV NUXT_PORT=3000
ENV FRONTEND_ENV=dev

RUN npm run install

# ARG FRONTEND_ENV=docker-dev
# ENV FRONTEND_ENV=${FRONTEND_DEV}



EXPOSE 3000

CMD ["/bin/bash", "simplerun.sh"]
