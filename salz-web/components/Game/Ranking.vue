<template>
  <div class="gameUIContainer ranking-ui">
    <div class="ranking-ui__header">
      <h2>Ranking</h2>
      <button class="closeUI" @click="closeUI">
        <b-icon icon="close" />
      </button>
    </div>
    <div class="ranking-ui__body">
      Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy
      nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat. Ut
      wisi enim ad minim veniam, quis nostrud exerci tation ullamcorper suscipit
      lobortis nisl ut aliquip ex ea commodo consequat. Duis autem vel eum
      iriure dolor in hendrerit in vulputate velit esse molestie consequat, vel
      illum dolore eu feugiat nulla facilisis at vero eros et accumsan et iusto
      odio dignissim qui blandit praesent luptatum zzril delenit augue duis
      dolore te feugait nulla facilisi. Nam liber tempor cum soluta nobis
      eleifend option congue nihil imperdiet doming id quod mazim placerat facer
      possim assum.
    </div>
  </div>
</template>

<style lang="scss" scoped>
.ranking-ui {
  padding: 10px;
  position: fixed;
  width: 80%;
  height: 80%;
  top: 10%;
  left: 10%;
  z-index: 1001;

  .ranking-ui__header {
    display: flex;
    justify-content: space-between;
  }

  .ranking-ui__body {
    columns: 2;
  }
}

@media screen and (max-width: 686px) {
  .ranking-ui {
    width: 90%;
    height: 90%;
    top: 5%;
    left: 5%;
  }
}

@media screen and (max-width: 400px) {
  .ranking-ui {
    width: 100%;
    height: 100%;
    top: 0;
    left: 0;

    .ranking-ui__body {
      columns: 1;
    }
  }
}
</style>

<script charset="utf-8">
import { mapState } from 'vuex';
export default {
  data() {
    return {};
  },
  computed: {
    ...mapState({
      showRanking: (state) => state.game.showRanking
    })
  },
  methods: {
    closeUI() {
      this.$store.dispatch('game/setShowRanking', !this.showRanking);
    }
  }
};
</script>
