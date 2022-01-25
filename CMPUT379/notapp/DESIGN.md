# CMPUT 379 Assignment 2
## Notification Application
Implemented enhancement: Server timestamps
### Protocol

#### Observer client
On initialization:
- Message sent: `S O watch_name`
- Explanation: SYNC OBSERVER WATCH_NAME
- Initial synchronization as observer, passing along the name of the watched file/folder.
  - Server reply:
    - `A`
    - Explanation: ACK
    - Replies with an acknowledgement to the client.

On new event:
- `M timestamp inotify_event::mask <filename>`
- Explanation: MESSAGE TIMESTAMP EVENT_MASK FILENAME
- Message from observer, sent on timestamp declaring the events for file.

On disconnect:
- `D`
- Explanation: DISCONNECT
- Tell server that this client is disconnecting, and should no longer have a thread maintained for.
    - Server reply:
        - `A`
        - Explanation: ACK
        - Replies with an acknowledgement to the client.

#### Listener client
On initialization:
- Message sent: `S W`
- Explanation: SYNC WATCHER
- Initial synchronization as client, notifying server of existence.
    - Server reply:
        - `A`
        - Explanation: ACK
        - Replies with an acknowledgement to the client.

On new event (from server to client):
- `M timestamp host target inotify_event::mask <filename>`
- Explanation: MESSAGE TIMESTAMP HOST WATCH_TARGET FILENAME EVENT_MASK
- Message from server about new event, with timestamp of event, the monitor target, the mask of the event and optionally the filename.

- `D target`
- Explanation: DISCONNECT WATCH_TARGET
- Message from the server telling the client that the observer for target has disconnected.

On disconnect:
- `D`
- Explanation: DISCONNECT
- Tell server that this client is disconnecting, and should no longer have a thread maintained for.
    - Server reply:
        - `A`
        - Explanation: ACK
        - Replies with an acknowledgement to the client.
