<template id="salz-game-view">
  <div id="salz-game-inner-view">
    <GameMenu />
    <FrameControl />
  </div>
</template>

<style lang="scss">
@import '~assets/css/colors';
#salz-game-inner-view {
  width: 100vw;
  height: 100vh;
}

.cellInfoContainer {
  padding: 20px;
  position: fixed;
}
</style>

<script charset="utf-8">
import hotkeys from 'hotkeys-js';

// Game Rendering
import { fullscreen } from '../lib/game/rendering/fullscreen';
import GameFrame from '../lib/game/rendering/gameFrame';
import Frame from '../lib/game/rendering/frame';
import { Color } from '../lib/game/colors.js';
import { sketchyMedoid } from '../lib/game/mathstuff';
import { EventBus } from '../lib/eventBus';

import GameMenu from '~/components/Game/UI/GameMenu';
import FrameControl from '~/components/Game/UI/FrameControl';

export default {
  components: {
    GameMenu,
    FrameControl
  },
  data() {
    return {
      index: []
    };
  },
  computed: {
    user() {
      return this.$store.state.login.username;
    },
    userid() {
      return this.$store.state.login.id;
    },
    currentFrameNumber() {
      return this.$store.getters['game/getActiveFrame'];
    },
    totalFrames() {
      return this.$store.getters['game/getFramesLength'];
    }
  },
  async asyncData(ctx) {
    return {
      index: await ctx.app.$framesRepo.index() // read frame info from api
    };
  },
  beforeMount() {
    this.$store.dispatch('login/grabToken');
  },
  async mounted() {
    const PIXI = await import('pixi.js');
    const Viewport = await import('pixi-viewport');
    const wrapper = document.querySelector('#salz-game-inner-view');

    // HACK -- This is a temporary solution
    // set a random color for each player
    const cellcolors = [
      Color.pink,
      Color.red,
      Color.orange,
      Color.yellow,
      Color.green,
      Color.turquiose,
      Color.cyan,
      Color.blue,
      Color.purple
    ];
    const playerColorDict = {};
    this.index.frames.forEach((frame) => {
      frame.forEach((player) => {
        const id = player.playerid;
        if (typeof playerColorDict[id] === 'undefined') {
          playerColorDict[id] =
            cellcolors[Math.floor(Math.random() * cellcolors.length)];
        }
        player.color = playerColorDict[id];
      });
    });

    this.$store.dispatch('game/setActiveFrame', 0);
    this.$store.dispatch('game/setFramesLength', this.index.frames);

    const frames = {};
    for (let i = 0; i < this.index.frames.length; i++) {
      frames[i] = new Frame(this.index.frames[i]);
    }

    // figure out the dimensions for the canvas
    const appWidth = Math.max(
      document.documentElement.clientWidth,
      window.innerWidth || 0
    );
    const appHeight = Math.max(
      document.documentElement.clientHeight,
      window.innerHeight || 0
    );

    const app = new PIXI.Application({
      width: appWidth,
      height: appHeight,
      backgroundColor: Color.black,
      resizeTo: wrapper,
      resolution: window.devicePixelRatio || 1
    });
    wrapper.appendChild(app.view);

    const vpWorldWidth = 1000;
    const vpWorldHeight = 1000;

    const viewport = new Viewport.Viewport({
      screenWidth: window.innerWidth,
      screenHeight: window.innerHeight,
      worldWidth: vpWorldWidth,
      worldHeight: vpWorldHeight,
      interaction: app.renderer.plugins.interaction
    });

    // Set the centre to be the mediod of player's cells
    // const user = JSON.parse(sessionStorage.getItem('user'));
    const playerID = parseInt(this.userid, 10);

    if (playerID == null && frames.length > 0) {
      viewport.moveCenter(0, 0);
    } else {
      // camera should point at middle of cluster.
      viewport.moveCenter(...sketchyMedoid(playerID, this.index.frames[0]));
    }

    app.stage.addChild(viewport);
    viewport
      .drag()
      .pinch()
      .wheel()
      .decelerate()
      .clampZoom({
        minWidth: vpWorldWidth / 5,
        minHeight: vpWorldHeight / 5,
        maxWidth: vpWorldWidth * 5,
        maxHeight: vpWorldHeight * 5
      });

    const gameFrame = await new GameFrame(
      frames[0],
      vpWorldWidth,
      vpWorldHeight
    );
    viewport.addChild(gameFrame);

    /**
     * Move viewport camera by dx and dy
     *
     * @param   {number}  dx  Pixels to move x by
     * @param   {number}  dy  Pixels to move y by
     */
    function moveViewport(dx, dy) {
      viewport.moveCenter(viewport.center.x + dx, viewport.center.y + dy);
    }

    // hotkeys
    const gameHotkeys = [
      {
        key: 'f',
        fn: () => {
          fullscreen(wrapper);
        }
      },
      {
        key: 'h',
        fn: () => {
          if (this.currentFrameNumber > 0) {
            this.$store.dispatch(
              'game/setActiveFrame',
              this.currentFrameNumber - 1
            );
            gameFrame.mountFrame(frames[this.currentFrameNumber]);
          }
        }
      },
      {
        key: 'l',
        fn: () => {
          if (this.currentFrameNumber < this.totalFrames - 1) {
            this.$store.dispatch(
              'game/setActiveFrame',
              this.currentFrameNumber + 1
            );
            gameFrame.mountFrame(frames[this.currentFrameNumber]);
          }
        }
      },
      {
        key: 'z',
        fn: () => {
          viewport.zoomPercent(0.5);
        }
      },
      {
        key: 'shift+z',
        fn: () => {
          viewport.zoomPercent(-0.5);
        }
      },
      {
        key: 'left',
        fn: (event) => {
          event.preventDefault();
          moveViewport(-10, 0);
        }
      },
      {
        key: 'shift+left',
        fn: (event) => {
          event.preventDefault();
          moveViewport(-50, 0);
        }
      },
      {
        key: 'up',
        fn: (event) => {
          event.preventDefault();
          moveViewport(0, -10);
        }
      },
      {
        key: 'shift+up',
        fn: (event) => {
          event.preventDefault();
          moveViewport(0, -50);
        }
      },
      {
        key: 'right',
        fn: (event) => {
          event.preventDefault();
          moveViewport(10, 0);
        }
      },
      {
        key: 'shift+right',
        fn: (event) => {
          event.preventDefault();
          moveViewport(50, 0);
        }
      },
      {
        key: 'down',
        fn: (event) => {
          event.preventDefault();
          moveViewport(0, 10);
        }
      },
      {
        key: 'shift+down',
        fn: (event) => {
          event.preventDefault();
          moveViewport(0, 50);
        }
      }
    ];

    // register each hotkey for Hotkey.js
    gameHotkeys.forEach((item) => {
      hotkeys(item.key, item.fn);
    });

    EventBus.$on('updateFrameIndex', () => {
      gameFrame.mountFrame(frames[this.currentFrameNumber]);
    });

    // wrapper.addEventListener('updateFrameIndex', (ev) => {
    //   frame = new Frame(frames[this.currentFrameNumber]);
    //   gameFrame.mountFrame(frame);
    // });

    // window.addEventListener('resize', (event) => {
    //   app.resize();

    //   const appWidth = Math.max(
    //     document.documentElement.clientWidth,
    //     window.innerWidth || 0
    //   );
    //   const appHeight = Math.max(
    //     document.documentElement.clientHeight,
    //     window.innerHeight || 0
    //   );
    //   btn.x = appWidth - btn.width - 20;
    //   btn.y = appHeight - btn.height - 20;
    // });
  }
};
</script>
