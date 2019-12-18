<template>
  <div class="gameMenu-container">
    <div class="gameUIContainer gameMenu" :class="{ active: menuIsActive }">
      <div v-for="(item, key) of menuItems" :key="key">
        <button v-if="item.type === 'action'" @click="item.action">
          {{ item.title }}
        </button>
        <nuxt-link v-if="item.type === 'link'" :to="item.to">
          {{ item.title }}
        </nuxt-link>
      </div>
    </div>
    <div
      class="gameUIContainer gameMenu-toggle"
      :class="{ active: menuIsActive }"
      @click="toggleMenuActivity"
    >
      <b-icon icon="menu" />
    </div>
  </div>
</template>

<style lang="scss" scoped>
.gameMenu-container {
  position: fixed;
  top: 75px;
  display: flex;
  z-index: 1002;
}

.gameMenu {
  display: grid;
  grid-template-columns: 33.33% 33.33% 33.33%;
  width: 30vw;
  min-width: 300px;
  max-width: 600px;
  padding: 30px;
  position: relative;
  margin-left: -30vw;
  text-transform: uppercase;
  opacity: 0;
  transition: all 0.5s ease-in-out;

  &.active {
    margin-left: 0;
    opacity: 1;
  }
}

.gameMenu-toggle {
  cursor: pointer;
  margin-left: 10px;
  padding: 10px;
  position: relative;
  display: flex;
  align-items: center;
  left: 0;
  transition: all 0.5s ease-in-out;

  &.active {
    margin-left: 0;
  }
}

@media screen and (max-width: 1300px) {
  .gameMenu {
    grid-template-columns: 50% 50%;
  }
}

@media screen and (max-width: 1000px) {
  .gameMenu {
    margin-left: -300px;
  }
}

@media screen and (max-width: 600px) {
  .gameMenu {
    grid-template-columns: 100%;
    max-height: 100px;
    overflow-y: scroll;
  }
}
</style>

<script charset="utf-8">
import { mapState } from 'vuex';
import { fullscreen } from '@/lib/game/rendering/fullscreen';

export default {
  name: 'GameMenu',
  data() {
    return {
      wrapper: null,
      menuItems: [
        { title: 'Guide', type: 'link', to: { name: 'guide' } },
        {
          title: 'Fullscreen',
          type: 'action',
          action: () => {
            fullscreen(this.wrapper);
          }
        },
        {
          title: 'Hotkeys',
          type: 'action',
          action: () => {
            this.$store.dispatch('game/setShowHotkeys', !this.showHotkeys);
          }
        },
        {
          title: 'Ranking',
          type: 'action',
          action: () => {
            this.$store.dispatch('game/setShowRanking', !this.showRanking);
          }
        },
        {
          title: 'Help',
          type: 'action',
          action: () => {
            this.$store.dispatch('game/setShowHelp', !this.showHelp);
          }
        },
        {
          title: 'Hide UI',
          type: 'action',
          action: () => {
            this.$store.dispatch('game/hideUI');
          }
        }
      ]
    };
  },
  computed: {
    ...mapState({
      menuIsActive: (state) => state.game.menuIsActive,
      showHotkeys: (state) => state.game.showHotkeys,
      showRanking: (state) => state.game.showRanking,
      showHelp: (state) => state.game.showHelp
    })
  },
  mounted() {
    this.wrapper = document.querySelector('#salz-game-inner-view');
  },
  methods: {
    toggleMenuActivity() {
      this.$store.dispatch('game/setMenuIsActive', !this.menuIsActive);
    }
  }
};
</script>
