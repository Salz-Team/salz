FROM node

COPY . /app
WORKDIR /app

# uses these env vars, can override the conf
ENV NUXT_HOST=0.0.0.0
ENV NUXT_PORT=3000

RUN npm install

EXPOSE 3000

CMD ["npm", "run", "dev"]
