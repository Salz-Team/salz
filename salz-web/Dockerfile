FROM node

COPY . /app
WORKDIR /app

# uses these env vars, can override the conf
ENV NUXT_HOST=0.0.0.0
ENV NUXT_PORT=3000
ENV NODE_ENV=production
ENV BASE_URL=https://salz.life

RUN npm install
RUN npm run build

EXPOSE 3000

CMD ["npm", "run", "start"]
