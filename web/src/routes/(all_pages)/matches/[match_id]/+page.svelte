<script lang="ts">
	import { onDestroy, onMount } from 'svelte';
  import { browser } from '$app/environment';
	import type { PageProps } from './$types';

	let { data }: PageProps = $props();

	let worker: Worker | null = $state(null);

	async function initWebWorker() {
		if (browser && window.Worker) {
			const TicTacToeWorker = await import('$workers/game-history-parsers/tic-tac-toe.ts?worker');
			worker = new TicTacToeWorker.default();
		}
	}

  function runWorker() {
    if (worker) {
      worker.postMessage({ gameHistory: data.gameHistory });
      worker.onmessage = (e) => {
        console.log(e.data.message);
      }
    }
  }

	function terminateWorker() {
		if (worker) {
			worker.terminate();
		}
	}

	onMount(() => {
		initWebWorker();
	});

  onDestroy(() => {
    terminateWorker();
  });
</script>

<button disabled={!Boolean(worker)} onclick={runWorker}>Run!</button>
