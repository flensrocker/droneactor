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

## game_registry

## game

## field

## player

## drone
