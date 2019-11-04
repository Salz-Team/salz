FROM node

COPY . /app
WORKDIR /app

RUN npm install

ENV NODE_ENV=production
ENV NUXT_HOST=0.0.0.0
ENV NUXT_PORT=3000
ENV BASE_URL=https://salz.life

RUN npm run build

EXPOSE 3000

CMD ["npm", "run", "start"]
