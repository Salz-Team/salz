<script lang="ts">
	import type { MatchResult as MatchResultModel } from '$lib/models';
	import { Temporal } from 'temporal-polyfill';
	import MatchResult from './MatchResult.svelte';

	export let matchResults: MatchResultModel[];

	$: localTimezoneId = Temporal.Now.timeZoneId();
</script>

{#each matchResults as matchResult}
	<MatchResult
		{matchResult}
		datetimeDisplay={matchResult.timestamp
			.toZonedDateTimeISO(localTimezoneId)
			.toPlainDateTime()
			.toLocaleString()}
	/>
{/each}
