<template>
  <section class="section">
    <div class="is-mobile">
      <!-- <card title="Free" icon="github-circle"> -->
      <!--   Open source on -->
      <!--   <a href="https://github.com/buefy/buefy"> -->
      <!--     GitHub -->
      <!--   </a> -->
      <!-- </card> -->

      <!-- <card title="Responsive" icon="cellphone-link"> -->
      <!--   <b class="has-text-grey"> -->
      <!--     Every -->
      <!--   </b> -->
      <!--   component is responsive -->
      <!-- </card> -->

      <!-- <card title="Modern" icon="alert-decagram"> -->
      <!--   Built with -->
      <!--   <a href="https://vuejs.org/"> -->
      <!--     Vue.js -->
      <!--   </a> -->
      <!--   and -->
      <!--   <a href="http://bulma.io/"> -->
      <!--     Bulma -->
      <!--   </a> -->
      <!-- </card> -->

      <!-- <card title="Lightweight" icon="arrange-bring-to-front"> -->
      <!--   No other internal dependency -->
      <!-- </card> -->
      <h1 class="title is-1">salz</h1>

      <h2 class="title is-2">Database</h2>

      <p>
        I am going to use sqlite. <br />
        Todo:
      </p>
      <ul>
        <li>setup database</li>
        <li>setup types and code</li>
        <ul>
          <li>players</li>
          <ul>
            <li>playerid</li>
            <li>botlocation :: string</li>
          </ul>

          <li>history</li>
          <ul>
            <li>map</li>
          </ul>
        </ul>
      </ul>

      <h2 class="title is-2">Directory Breakup</h2>

      <h3 class="title is-3">Board</h3>

      <p>Responsible for board geometry</p>

      <h3 class="title is-3">External Process Handler</h3>

      <p>
        Responsible for handling an external process <strong>Interface</strong>:
        - createExternalProcess - timedCallandResponse - replaceExecutable
      </p>

      <h3 class="title is-3">PlayerBot Handler</h3>

      <p>
        Responsible for the PlayerBot Interface Todo: - create status for the
        player bot
      </p>

      <h3 class="title is-3">Player</h3>

      <p>
        Responsible for player game information: - what a player can see - what
        a player can effect - the starting location of a player
        <strong>Interface</strong>:
      </p>

      <h3 class="title is-3">Game</h3>

      <p>
        Responsible for game logic: - gameStep :: going from one state to the
        next - gameLoop :: tying together gameStep with player and
        playerIOHandler
      </p>

      <h2 class="title is-2">Todo</h2>

      <ul>
        <li>[ ] Save the game to json and to database</li>
        <li>[ ] Create the resource to be mined (to allow for more power)</li>
        <li>[ ] Allow creation of new players</li>
      </ul>
    </div>
  </section>
</template>

<script>
// import Card from '~/components/Card';

export default {
  name: 'HomePage'

  // components: {
  //   Card
  // }
};
</script>
