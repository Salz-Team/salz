<template>
  <div v-if="showHotkeys" class="gameUIContainer hotkeys-ui">
    <div class="hotkey-ui__header">
      <h2>Hotkeys</h2>
      <button class="closeUI" @click="closeUI">
        <b-icon icon="close" />
      </button>
    </div>
    <div class="hotkey-ui__body">
      <div v-for="(hotkey, key) of hotkeys" :key="key">
        <b>{{ hotkey.key }}</b
        >:
        {{ hotkey.description }}
      </div>
    </div>
  </div>
</template>

<style lang="scss" scoped>
.hotkeys-ui {
  padding: 10px;
  position: fixed;
  width: 80vw;
  height: 80vh;
  top: 10%;
  left: 10%;
  z-index: 1001;

  .hotkey-ui__header {
    display: flex;
    justify-content: space-between;
  }

  .hotkey-ui__body {
    columns: 3;
  }
}

@media screen and (max-width: 686px) {
  .hotkeys-ui {
    width: 90vw;
    height: 90vh;
    top: 5%;
    left: 5%;
    overflow-y: scroll;

    .hotkey-ui__body {
      columns: 2;
    }
  }
}

@media screen and (max-width: 400px) {
  .hotkeys-ui {
    width: 100vw;
    height: 100vh;
    top: 0;
    left: 0;

    .hotkey-ui__body {
      columns: 1;
    }
  }
}
</style>

<script charset="utf-8">
import { mapState } from 'vuex';
import hotkeys from 'hotkeys-js';
import { fullscreen } from '~/lib/game/rendering/fullscreen';

export default {
  props: {
    viewport: {
      type: Object,
      default: null
    }
  },
  data() {
    return {
      hotkeys: [
        {
          key: 'f',
          description: 'Toggle fullscreen',
          fn: () => {
            fullscreen();
          }
        },
        {
          key: 'h',
          description: 'Backward by 1 frame',
          fn: () => {
            this.$store.dispatch('game/goToPrevFrame');
          }
        },
        {
          key: 'shift+h',
          description: 'Toggle help page',
          fn: () => {
            this.$store.dispatch('game/setShowHelp', !this.showHelp);
          }
        },
        {
          key: 'shift+k',
          description: 'Toggle hotkey cheatsheet',
          fn: () => {
            this.$store.dispatch('game/setShowHotkeys', !this.showHotkeys);
          }
        },
        {
          key: 'l',
          description: 'Forward by 1 frame',
          fn: () => {
            this.$store.dispatch('game/goToNextFrame');
          }
        },
        {
          key: 'shift+r',
          description: 'Toggle Ranking page',
          fn: () => {
            this.$store.dispatch('game/setShowRanking', !this.showRanking);
          }
        },
        {
          key: 'shift+s',
          description: 'Toggle UI',
          fn: () => {
            if (this.hideUI) {
              this.$store.dispatch('game/showUI');
            } else {
              this.$store.dispatch('game/hideUI');
            }
          }
        },
        {
          key: 'z',
          description: 'Zoom in',
          fn: () => {
            this.viewport.zoomPercent(0.5);
          }
        },
        {
          key: 'shift+z',
          description: 'Zoom out',
          fn: () => {
            this.viewport.zoomPercent(-0.5);
          }
        },
        {
          key: 'left',
          description: 'Move viewport leftwards',
          fn: (event) => {
            event.preventDefault();
            this.viewport.moveViewport(-10, 0);
          }
        },
        {
          key: 'shift+left',
          description: 'Move viewport leftwards (more increment)',
          fn: (event) => {
            event.preventDefault();
            this.viewport.moveViewport(-50, 0);
          }
        },
        {
          key: 'up',
          description: 'Move viewport upwards',
          fn: (event) => {
            event.preventDefault();
            this.viewport.moveViewport(0, -10);
          }
        },
        {
          key: 'shift+up',
          description: 'Move viewport upwards (more increment)',
          fn: (event) => {
            event.preventDefault();
            this.viewport.moveViewport(0, -50);
          }
        },
        {
          key: 'right',
          description: 'Move viewport rightwards',
          fn: (event) => {
            event.preventDefault();
            this.viewport.moveViewport(10, 0);
          }
        },
        {
          key: 'shift+right',
          description: 'Move viewport rightwards (more increment)',
          fn: (event) => {
            event.preventDefault();
            this.viewport.moveViewport(50, 0);
          }
        },
        {
          key: 'down',
          description: 'Move viewport downwards',
          fn: (event) => {
            event.preventDefault();
            this.viewport.moveViewport(0, 10);
          }
        },
        {
          key: 'shift+down',
          description: 'Move viewport downwards (more increment)',
          fn: (event) => {
            event.preventDefault();
            this.viewport.moveViewport(0, 50);
          }
        }
      ]
    };
  },
  computed: {
    ...mapState({
      hideUI: (state) => state.game.hideUI,
      showHotkeys: (state) => state.game.showHotkeys,
      showRanking: (state) => state.game.showRanking,
      showHelp: (state) => state.game.showHelp
    })
  },
  created() {
    this.hotkeys.forEach((item) => {
      hotkeys(item.key, item.fn);
    });
  },
  methods: {
    closeUI() {
      this.$store.dispatch('game/setShowHotkeys', !this.showHotkeys);
    }
  }
};
</script>
