FROM node

COPY . /app
WORKDIR /app

RUN npm run install

ARG FRONTEND_ENV=docker-dev
ENV FRONTEND_ENV=${FRONTEND_DEV}

EXPOSE 3000

CMD ["/bin/bash", "run.sh"]
