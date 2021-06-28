# Modules

## player_register_handler
Http-Post request with content-type application/json
```
POST /api/player/register

interface RegisterPlayerRequest {
  game_name: string;
  player_name: string;
}
```

Sets cookie with JWT
```
interface PlayerCookie {
  game_name: string;
  player_name: string;
  player_id: string;
}
```

## player_socket_handler
Upgrade to websocket, needs player-cookie

```
GET /api/player/connect
```

```
interface SocketMessage<TMessage, TPayload> {
  message: TMessage;
  payload: TPayload;
}
```

| receives | payload | sends | remarks |
| --- | --- | --- | --- |
| game_joined | game_pid, player_id, player_name, player_pid |
| game_full | game_name |
| game_state | game.state without pids |

## game_registry

### State

- games
  - game_name
  - game_pid
  - players
    - player_id

### Messages

| receives | payload | sends | remarks |
| --- | --- | --- | --- |
| join_game | player_id, player_name | game_joined \| game_full => player_socket_handler | if not exists creates game and player |

## game

### State
- game_name
- game_size
- current_tick
- last_tick_time
- players
  - player_id
  - player_pid
  - player_name
  - queen_field_id
  - queen_stress_level
  - queen_next_drone_type
- fields
  - field_id
  - field_pid
  - coordinates
  - drones
    - drone_id
    - drone_type
    - player_id

### Messages

| receives | payload | sends | remarks |
| --- | --- | --- | --- |
| player_joined | player_id, player_name
| field_state_changed | tick_count, field_id, drones | tick* => all  players | * when all fields reported, timeout = (tick_interval - (current_time - last_tick_time))
| move_started | tick_count, player_id | eval_drones* => all fields | * when all players reported
| restart_game | player_id | game_restarted* => all players | * when all players want restart

## player

### State
- player_id
- player_name
- game_pid
- queen_field_id
- queen_stress_level
- queen_next_drone_type
- current_tick
- drones
  - drone_id
  - drone_pid

### Messages

| receives | payload | sends | remarks |
| --- | --- | --- | --- |
| tick | tick_count | tick => all drones | create new drone if `ticket_count rem 3 = 0` |
| move_started | tick_count, drone_id | move_started* => game | * when all drones reported |
| game_restarted | | | kill all drones, reset state |

## field

### State
- field_id
- coordinates
- game_pid
- current_tick
- drones
  - player_id
  - drone_id
  - drone_pid
  - drone_type

### Messages

| receives | payload | sends | remarks |
| --- | --- | --- | --- |
| get_drones | tick_count | drones_on_field => sender | |
| start_move | tick_count, target_field, player_id, drone_id, drone_type | drone_moved => target_field | remove drone from state |
| drone_moved | tick_count, player_id, drone_id, drone_type | | add drone to state |
| eval_drones | tick_count | move_ended => all drones; field_state_changed => game | |
| game_restarted | | | clear state

## drone

### State
- game_pid
- player_id
- palyer_pid
- drone_type:
  - soldier
  - worker_empty
  - worker_loaded
  - sniffer
  - infector
  - seeker
- queen_field_id
- current_field_id
- fields_in_sight
- current_tick
- messages_to_expect

### Messages

| receives | payload | sends | new expected | remarks |
| --- | --- | --- | --- | --- |
| tick | tick_count | get_drones => fields in sight; calc_move* => self | drones_on_field, calc_move | * with timeout |
| drones_on_field | tick_count, field_id, [{player_id, drone_type, count}] | calc_move* => self | drones_on_field, calc_move | * send calc_move without timeout if all fields returned |
| calc_move | tick_count | start_move => selected field; move_started => player | move_ended, died |
| move_ended | tick_count, field, new_drone_type | | tick |
| died | | | | stop self |
