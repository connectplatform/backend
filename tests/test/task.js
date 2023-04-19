module.exports = function(p){
  p
  .connect('user1')
  .connect('user2')
  .connect('user3')
  //send message from user1 to user 2
  .then()
  .send('user1', 'Message', global.entityHelper.generateMessage(map.MessageFeedType.chat, '000000000000000000000002', 'Message 1'), response => {
		p.equals(response.modelName, 'MessageResp')
  })
  .then()
  .timeout(500)
  //check create task
  .then()
  .send('user1', 'CreateTask', {
    task: {
      payload: 'Task 1',
      assignee: '000000000000000000000002',
      sourceMessageIds: ['000000000000000000000011'],
      feedType: map.MessageFeedType.chat,
      feedId: '000000000000000000000002',
      deadline: 1500000000
    }
  }, response => {
    p.set('taskId1', response.task.id)
    p.equals(response.task.status, 'todo')
    p.equals(response.task.payload, 'Task 1')
    p.equals(response.task.reporter, '000000000000000000000001')
    p.equals(response.task.assignee, '000000000000000000000002')
    p.equals(response.task.status, 'todo')
    p.equals(response.task.feedType, map.MessageFeedType.chat)
    p.equals(response.task.feedId, '000000000000000000000002')
    p.equals(response.task.deadline, 1500000000)
    p.deepEquals(response.task.sourceMessageIds, ['000000000000000000000011'])
  })
  .and()
  .wait('user1', 'Update', response => {
    p.equals(response.update.type, map.UpdateType.task)
    p.equals(!!response.update.task.id, true)
    p.equals(response.update.task.payload, 'Task 1')
    p.equals(response.update.task.reporter, '000000000000000000000001')
    p.equals(response.update.task.assignee, '000000000000000000000002')
    p.equals(response.update.task.feedType, map.MessageFeedType.chat)
    p.equals(response.update.task.feedId, '000000000000000000000002')
    p.equals(response.update.task.deadline, 1500000000)
    p.equals(response.update.task.status, 'todo')
  })
  .and()
  .wait('user2', 'Update', response => {
    p.equals(response.update.type, map.UpdateType.task)
    p.equals(!!response.update.task.id, true)
    p.equals(response.update.task.payload, 'Task 1')
    p.equals(response.update.task.reporter, '000000000000000000000001')
    p.equals(response.update.task.assignee, '000000000000000000000002')
    p.equals(response.update.task.feedType, map.MessageFeedType.chat)
    p.equals(response.update.task.feedId, '000000000000000000000001')
    p.equals(response.update.task.status, 'todo')
  })
  //check feeds after create task
  .then()
  .send('user1', 'Task', {status: 'todo'}, response => {
    p.equals(response.total, 1)
    p.equals(response.tasks.length, 1)
    p.equals(response.tasks[0].id, get('taskId1'))
    p.equals(response.tasks[0].payload, 'Task 1')
    p.equals(response.tasks[0].reporter, '000000000000000000000001')
    p.equals(response.tasks[0].assignee, '000000000000000000000002')
    p.equals(response.tasks[0].deadline, 1500000000)
    p.equals(response.tasks[0].feedType, map.MessageFeedType.chat)
    p.equals(response.tasks[0].feedId, '000000000000000000000002')
  })
  .then()
  .send('user2', 'Task', {status: 'todo'}, response => {
    p.equals(response.total, 1)
    p.equals(response.tasks.length, 1)
    p.equals(response.tasks[0].id, get('taskId1'))
    p.equals(response.tasks[0].payload, 'Task 1')
    p.equals(response.tasks[0].reporter, '000000000000000000000001')
    p.equals(response.tasks[0].assignee, '000000000000000000000002')
    p.equals(response.tasks[0].feedType, map.MessageFeedType.chat)
    p.equals(response.tasks[0].feedId, '000000000000000000000001')
  })
  .then()
  .send('user1', 'Task', {status: 'done'}, response => {
    p.equals(response.total, 0)
    p.equals(response.tasks.length, 0)
  })
  .then()
  .send('user2', 'Task', {status: 'done'}, response => {
    p.equals(response.total, 0)
    p.equals(response.tasks.length, 0)
  })
  //check acl
  .then()
  .send('user3', 'UpdateTaskStatus', {id: get('taskId1'), status: 'done'}, response => {
    p.equals(response.modelName, 'ErrorResp')
  })
  .then()
  .send('user3', 'UpdateTask', {id: get('taskId1'), payload: 'Task 111'}, response => {
    p.equals(response.modelName, 'ErrorResp')
  })
  .then()
  .send('user3', 'AssignTask', {id: get('taskId1'), assignee: '000000000000000000000004'}, response => {
    p.equals(response.modelName, 'ErrorResp')
  })
  .then()
  .send('user3', 'DeleteTask', {id: get('taskId1')}, response => {
    p.equals(response.modelName, 'ErrorResp')
  })
  //update status to "done"
  .then()
  .send('user1', 'UpdateTaskStatus', {id: get('taskId1'), status: 'done'}, response => {
    p.equals(response.modelName, 'UpdateTaskStatusResp')
  })
  .and()
  .wait('user1', 'Update', response => {
    p.equals(response.update.type, map.UpdateType.task)
    p.equals(response.update.task.id, get('taskId1'))
    p.equals(response.update.task.status, 'done')
  })
  .and()
  .wait('user2', 'Update', response => {
    p.equals(response.update.type, map.UpdateType.task)
    p.equals(response.update.task.id, get('taskId1'))
    p.equals(response.update.task.status, 'done')
  })
  .then()
  .send('user1', 'Task', {status: 'todo'}, response => {
    p.equals(response.total, 0)
    p.equals(response.tasks.length, 0)
  })
  .then()
  .send('user2', 'Task', {status: 'todo'}, response => {
    p.equals(response.total, 0)
    p.equals(response.tasks.length, 0)
  })
  .then()
  .send('user1', 'Task', {status: 'done'}, response => {
    p.equals(response.total, 1)
    p.equals(response.tasks.length, 1)
    p.equals(response.tasks[0].id, get('taskId1'))
  })
  .then()
  .send('user2', 'Task', {status: 'done'}, response => {
    p.equals(response.total, 1)
    p.equals(response.tasks.length, 1)
    p.equals(response.tasks[0].id, get('taskId1'))
  })
  //check that system user notified about closed task
  // .then()
  // .send('user1', 'ChatUpdates', {syncTime: 0}, response => {
  //   p.e_quals(response.updates.length, 1)
  //   p.e_quals(response.updates[0].top.systemMessageType, 'task.message.done')
  //   p.d_eepEquals(response.updates[0].top.systemMessageParams, ['<user:000000000000000000000002>', 'Task 1'])
  // })
  //check update task
  .then()
  .send('user1', 'UpdateTask', {id: get('taskId1'), payload: 'Task 1 Edited'}, response => {
    p.equals(response.modelName, 'UpdateTaskResp')
  })
  .and()
  .wait('user1', 'Update', response => {
    p.equals(response.update.type, map.UpdateType.task)
    p.equals(response.update.task.id, get('taskId1'))
    p.equals(response.update.task.payload, 'Task 1 Edited')
  })
  .and()
  .wait('user2', 'Update', response => {
    p.equals(response.update.type, map.UpdateType.task)
    p.equals(response.update.task.id, get('taskId1'))
    p.equals(response.update.task.payload, 'Task 1 Edited')
  })
  .then()
  .send('user1', 'Task', {status: 'done'}, response => {
    p.equals(response.total, 1)
    p.equals(response.tasks.length, 1)
    p.equals(response.tasks[0].id, get('taskId1'))
    p.equals(response.tasks[0].payload, 'Task 1 Edited')
  })
  .then()
  .send('user2', 'Task', {status: 'done'}, response => {
    p.equals(response.total, 1)
    p.equals(response.tasks.length, 1)
    p.equals(response.tasks[0].id, get('taskId1'))
    p.equals(response.tasks[0].payload, 'Task 1 Edited')
  })
  //check assign task
  .then()
  .send('user1', 'AssignTask', {id: get('taskId1'), assignee: '000000000000000000000003'}, response => {
    p.equals(response.modelName, 'AssignTaskResp')
  })
  .and()
  .wait('user1', 'Update', response => {
    p.equals(response.update.type, map.UpdateType.task)
    p.equals(response.update.task.id, get('taskId1'))
    p.equals(response.update.task.reporter, '000000000000000000000001')
    p.equals(response.update.task.assignee, '000000000000000000000003')
  })
  .and()
  .wait('user3', 'Update', response => {
    p.equals(response.update.type, map.UpdateType.task)
    p.equals(response.update.task.id, get('taskId1'))
    p.equals(response.update.task.reporter, '000000000000000000000001')
    p.equals(response.update.task.assignee, '000000000000000000000003')
  })
  .and()
  .wait('user2', 'Update', response => {
    p.equals(response.update.type, map.UpdateType.deleteTasks)
    p.deepEquals(response.update.ids, [get('taskId1')])
  })
  .then()
  .send('user1', 'Task', {status: 'done'}, response => {
    p.equals(response.total, 1)
    p.equals(response.tasks.length, 1)
    p.equals(response.tasks[0].id, get('taskId1'))
    p.equals(response.tasks[0].reporter, '000000000000000000000001')
    p.equals(response.tasks[0].assignee, '000000000000000000000003')
  })
  .then()
  .send('user3', 'Task', {status: 'done'}, response => {
    p.equals(response.total, 1)
    p.equals(response.tasks.length, 1)
    p.equals(response.tasks[0].id, get('taskId1'))
    p.equals(response.tasks[0].reporter, '000000000000000000000001')
    p.equals(response.tasks[0].assignee, '000000000000000000000003')
  })
  .then()
  .send('user2', 'Task', {status: 'todo'}, response => {
    p.equals(response.total, 0)
    p.equals(response.tasks.length, 0)
  })
  .then()
  .send('user2', 'Task', {status: 'done'}, response => {
    p.equals(response.tasks.length, 0)
  })
  //check delete task
  .then()
  .send('user1', 'DeleteTask', {id: get('taskId1')}, response => {
    p.equals(response.modelName, 'DeleteTaskResp')
  })
  .and()
  .wait('user1', 'Update', response => {
    p.equals(response.update.type, map.UpdateType.deleteTasks)
    p.deepEquals(response.update.ids, [get('taskId1')])
  })
  .and()
  .wait('user3', 'Update', response => {
    p.equals(response.update.type, map.UpdateType.deleteTasks)
    p.deepEquals(response.update.ids, [get('taskId1')])
  })
  .then()
  .send('user1', 'Task', {status: 'done'}, response => {
    p.equals(response.total, 0)
    p.equals(response.tasks.length, 0)
  })
  //check attach task to room
  .then()
  .send('user1', 'Room', {room: {topic: 'Room 1', members: ['000000000000000000000002', '000000000000000000000003']}}, response => {
    p.set('roomId', response.room.id)
    p.equals(response.modelName, 'RoomResp')
  })
  .then()
  .send('user1', 'CreateTask', {
    task: {
      payload: 'Task 2',
      assignee: '000000000000000000000002',
      feedType: map.MessageFeedType.room,
      feedId: get('roomId')
    }
  }, response => {
    p.set('taskId2', response.task.id)
    p.equals(response.task.payload, 'Task 2')
    p.equals(response.task.feedType, map.MessageFeedType.room)
    p.equals(response.task.feedId, get('roomId'))
    p.equals(response.task.deadline, 0)
  })
  .then()
  .send('user3', 'CreateTask', {
    task: {
      payload: 'Task 3',
      assignee: '000000000000000000000002'
    }
  }, response => {
    p.set('taskId3', response.task.id)
    p.equals(response.task.payload, 'Task 3')
  })
  .then()
  .send('user2', 'Task', {status: 'todo'}, response => {
    p.equals(response.tasks.length, 2)
    p.equals(response.tasks[0].id, get('taskId3'))
    p.equals(response.tasks[1].id, get('taskId2'))
  })
  .then()
  .send('user2', 'Task', {status: 'todo', feedType: map.MessageFeedType.room, feedId: get('roomId')}, response => {
    p.equals(response.tasks.length, 1)
    p.equals(response.tasks[0].id, get('taskId2'))
  })
  //check ACL: user3 is in group chat
  .then()
  .then()
  .send('user3', 'Task', {status: 'todo', feedType: map.MessageFeedType.room, feedId: get('roomId')}, response => {
    p.equals(response.tasks.length, 1)
    p.equals(response.tasks[0].id, get('taskId2'))
  })
  //check ACL: user4 is not assignee/reporter in any task and also not in group chat
  .then()
  .connect('user4')
  .then()
  .send('user4', 'Task', {status: 'todo'}, response => {
    p.equals(response.tasks.length, 0)
  })
  .then()
  .send('user4', 'Task', {status: 'todo', feedType: map.MessageFeedType.room, feedId: get('roomId')}, response => {
    p.equals(response.modelName, 'ErrorResp')
    p.equals(response.code, map.ErrorCode.permissionDenied)
  })
  // //check that system user notifies when task without feedId is done
  // .then()
  // .send('user2', 'UpdateTaskStatus', {id: get('taskId3'), status: 'done'}, response => {
  //   p.e_quals(response.modelName, 'UpdateTaskStatusResp')
  // })
  // .then()
  // .send('user3', 'ChatUpdates', {syncTime: 0}, response => {
  //   p.e_quals(response.updates.length, 1)
  //   p.e_quals(response.updates[0].top.systemMessageType, 'task.message.done')
  //   p.d_eepEquals(response.updates[0].top.systemMessageParams, ['<user:000000000000000000000002>', 'Task 3'])
  // })
}
