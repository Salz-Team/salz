<template>
  <div class="gameMenu-container">
    <div
      :class="{ active: menuIsActive }"
      @click="toggleMenuActivity"
      class="gameUIContainer gameMenu-toggle"
    >
      <transition name="fade" mode="out-in">
        <b-icon key="menuOpen" v-if="!menuIsActive" icon="menu" />
        <b-icon key="menuClose" v-else icon="close" />
      </transition>
    </div>
    <div :class="{ active: menuIsActive }" class="gameUIContainer gameMenu">
      <div v-for="(item, key) of menuItems" :key="key">
        <button v-if="item.type === 'action'" @click="item.action" class="menuItem">
          <b-icon :icon="item.icon" size="is-large" />
          {{ item.title }}
        </button>
        <nuxt-link v-else-if="item.type === 'link'" :to="item.to" class="menuItem">
          <b-icon :icon="item.icon" size="is-large" />
          {{ item.title }}
        </nuxt-link>
      </div>
    </div>
  </div>
</template>

<style lang="scss" scoped>
@import '~assets/css/bulma-configs';
.gameMenu-container {
  position: fixed;
  top: 75px;
  width: 100vw;
  display: flex;
  flex-direction: column;
  justify-content: center;
  align-items: center;
  z-index: 1002;
}

.gameMenu {
  display: grid;
  grid-template-columns: 33.33% 33.33% 33.33%;
  row-gap: 15px;
  justify-items: center;
  width: 30vw;
  min-width: 300px;
  max-width: 600px;
  padding: 30px 10px;
  position: absolute;
  top: -100vh;
  text-transform: uppercase;
  opacity: 0;
  transition: all 0.5s ease-in-out;

  &.active {
    top: 75px;
    opacity: 1;
  }

  .menuItem {
    width: 100px;
    height: 100px;
    font-size: $size-6;
    display: flex;
    flex-direction: column;
    justify-content: center;
    align-items: center;
    background-image: url('/assets/images/icon-frame.svg');
    background-repeat: no-repeat;
    background-size: 100px 100px;

    &:link,
    &:visited,
    &:focus,
    &:hover {
      color: $white;
    }
  }
}

.gameMenu-toggle {
  background: $primary;
  cursor: pointer;
  padding: 10px;
  width: 50px;
  height: 50px;
  border-radius: 50%;
  position: relative;
  display: flex;
  align-items: center;
  justify-content: center;
  transition: all 0.5s ease-in-out;
}

@media screen and (max-width: 1300px) {
  .gameMenu {
    grid-template-columns: 50% 50%;
  }
}

/*
@media screen and (max-width: 1000px) {
  .gameMenu {
  }
}
*/

@media screen and (max-width: 600px) {
  .gameMenu {
    max-height: 300px;
    overflow-y: scroll;
  }

  .desktop-only {
    display: none;
  }
}

@media screen and (max-width: 400px) {
  .gameMenu-container {
    z-index: 999;
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
        {
          title: 'Guide',
          icon: 'book',
          type: 'link',
          to: { name: 'guide' },
        },
        {
          title: 'Fullscreen',
          icon: 'fullscreen',
          type: 'action',
          action: () => {
            fullscreen(this.wrapper);
          },
        },
        {
          title: 'Hotkeys',
          icon: 'keyboard-variant',
          type: 'action',
          action: () => {
            this.$store.dispatch('game/setShowHotkeys', !this.showHotkeys);
          },
        },
        {
          title: 'Ranking',
          icon: 'crown',
          type: 'action',
          action: () => {
            this.$store.dispatch('game/setShowRanking', !this.showRanking);
          },
        },
        {
          title: 'Help',
          icon: 'help',
          type: 'action',
          action: () => {
            this.$store.dispatch('game/setShowHelp', !this.showHelp);
          },
        },
        {
          title: 'Hide UI',
          icon: 'view-quilt',
          type: 'action',
          action: () => {
            this.$store.dispatch('game/hideUI');
          },
        },
      ],
    };
  },
  computed: {
    ...mapState({
      menuIsActive: state => state.game.menuIsActive,
      showHotkeys: state => state.game.showHotkeys,
      showRanking: state => state.game.showRanking,
      showHelp: state => state.game.showHelp,
    }),
  },
  mounted() {
    this.wrapper = document.querySelector('#salz-game-inner-view');
  },
  methods: {
    toggleMenuActivity() {
      this.$store.dispatch('game/setMenuIsActive', !this.menuIsActive);
    },
  },
};
</script>
