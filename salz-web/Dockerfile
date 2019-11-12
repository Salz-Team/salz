FROM node

COPY ./package.json ./package-lock.json /app
WORKDIR /app

RUN npm install

ENV NODE_ENV=production
ENV NUXT_HOST=0.0.0.0
ENV NUXT_PORT=3000

COPY . /app

RUN npm run build

EXPOSE 3000

CMD ["npm", "run", "start"]
