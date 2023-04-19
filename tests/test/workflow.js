const _ = require('underscore');

const workflowConfig = {
	version: 1,
	target: 'user',
	attributes: ['attr1', 'attr2', 'attr3'],
	startSteps: ['step1'],
	steps: [
		{
			modelName: 'WorkflowStepEntity',
			name: 'step1',
			allowedSteps: ['step2'],
			requiredAttributes: ['attr1'],
			setAttributesBefore: [{modelName: 'WorkflowAttrEntity', name: 'attr2', value: 'Value 2'}],
			setAttributesAfter: [{modelName: 'WorkflowAttrEntity', name: 'attr3', value: 'Value 3'}],
		},
		{
			modelName: 'WorkflowStepEntity',
			name: 'step2',
			allowedSteps: ['step3'],
			requiredAttributes: [],
			setAttributesAfter: [],
			setAttributesBefore: [],
		},
		{
			modelName: 'WorkflowStepEntity',
			name: 'step3',
			allowedSteps: ['ended'],
			requiredAttributes: [],
			setAttributesAfter: [],
			setAttributesBefore: [],
		},
	]
};

const workflowConfigV2 = _.clone(workflowConfig);
workflowConfigV2.startSteps = ['step1', 'step2'];
workflowConfigV2.version = 2;

module.exports = function(p){
  p
  .connect('user1')
  //check that customer cant create workflow
  .then()
  .sendHttp(null, 'RequestVerification', {phone: '+380930000011'}, function(){})
  .then()
  .sendHttp(null, 'ConfirmVerification', {phone: '+380930000011', smsCode: '111', deviceId: '3310', deviceName: 'nokia', os: map.Platform.ios}, function(response){
    p.equals(!!response.token, true)
    p.set('newUser', response.token)
  })
  .then()
  .connect(get('newUser'))
  .send(get('newUser'), 'UpdateUser', {user: {name: 'NewUser'}}, function(response){
    p.equals(response.user.name, 'NewUser')
    p.set('newUserId', response.user.id)
  })
  .then()
  .send(get('newUser'), 'CreateWorkflow', {name: 'workflow1', json: JSON.stringify(workflowConfig)}, response => {
    p.equals(response.modelName, 'ErrorResp')
    p.equals(response.code, map.ErrorCode.permissionDenied)
  })
  //check create workflow
  then()
  .send('user1', 'CreateWorkflow', {name: 'workflow1', json: JSON.stringify(workflowConfig)}, response => {
    // console.log(util.inspect(response.workflow, false, 99))
    p.equals(response.modelName, 'CreateWorkflowResp')
    p.equals(response.workflow.name, 'workflow1')
    p.equals(response.workflow.version, 1)
    p.deepEquals(response.workflow.steps, workflowConfig.steps)
    p.deepEquals(response.workflow.startSteps, workflowConfig.startSteps)
    p.deepEquals(response.workflow.attributes, workflowConfig.attributes)
  })
  //check that you can't update workflow config with the same version
  .then()
  .send('user1', 'CreateWorkflow', {name: 'workflow1', json: JSON.stringify(workflowConfig)}, response => {
    p.equals(response.modelName, 'ErrorResp')
    p.equals(response.code, map.ErrorCode.invalidMessage)
    p.equals(response.message, 'Workflow version already exists')
  })
  //check update workflow
  .then()
  .send('user1', 'CreateWorkflow', {name: 'workflow1', json: JSON.stringify(workflowConfigV2)}, response => {
    p.equals(response.modelName, 'CreateWorkflowResp')
    p.equals(response.workflow.name, 'workflow1')
    p.equals(response.workflow.version, 2)
    p.deepEquals(response.workflow.steps, workflowConfig.steps)
    p.deepEquals(response.workflow.startSteps, workflowConfigV2.startSteps)
    p.deepEquals(response.workflow.attributes, workflowConfig.attributes)
  })
  //check that without require attributes workflow doesn't start
  .then()
  .send('user1', 'StartWorkflow', {name: 'workflow1', entityId: '000000000000000000000002', step: 'step1'}, response => {
    p.equals(response.modelName, 'ErrorResp')
    p.equals(response.code, map.ErrorCode.invalidMessage)
  })
  //check start workflow
  .then()
  .send('user1', 'StartWorkflow', {
    name: 'workflow1',
    entityId: '000000000000000000000002',
    step: 'step1',
    setAttributes: [{ name: 'attr1', value: 'Value 1' }],
  }, response => {
    p.equals(response.state.name, 'workflow1')
    p.equals(response.state.version, 2)
    p.equals(response.state.step, 'step1')
    p.equals(response.state.active, true)
    p.deepEquals(response.state.attributes,
      [
        { modelName: 'WorkflowAttrEntity', name: 'attr1', value: 'Value 1' },
        { modelName: 'WorkflowAttrEntity', name: 'attr2', value: 'Value 2' },
        { modelName: 'WorkflowAttrEntity', name: 'attr3', value: null },
      ]
    )
  })
  //check get workflow state
  .then()
  .send('user1', 'GetWorkflowState', {name: 'workflow1', entityId: '000000000000000000000002'}, response => {
    p.equals(response.modelName, 'GetWorkflowStateResp')
    p.equals(response.state.name, 'workflow1')
    p.equals(response.state.version, 2)
    p.equals(response.state.step, 'step1')
    p.equals(response.state.active, true)
    p.deepEquals(response.state.attributes,
      [
        { modelName: 'WorkflowAttrEntity', name: 'attr1', value: 'Value 1' },
        { modelName: 'WorkflowAttrEntity', name: 'attr2', value: 'Value 2' },
        { modelName: 'WorkflowAttrEntity', name: 'attr3', value: null },
      ]
    )
  })
  //check that transition to restricted step fails
  .then()
  .send('user1', 'TransitWorkflow', {name: 'workflow1', entityId: '000000000000000000000002', step: 'step3'}, response => {
    p.equals(response.modelName, 'ErrorResp')
    p.equals(response.code, map.ErrorCode.invalidMessage)
  })
  //check transit workflow (step1 -> step2)
  .then()
  .send('user1', 'TransitWorkflow', {
    name: 'workflow1',
    entityId: '000000000000000000000002',
    step: 'step2',
    setAttributes: [{ name: 'newAttr', value: 'New Value' }],
  }, response => {
    p.equals(response.modelName, 'TransitWorkflowResp')
    p.equals(response.state.name, 'workflow1')
    p.equals(response.state.version, 2)
    p.equals(response.state.step, 'step2')
    p.equals(response.state.active, true)
    p.deepEquals(response.state.attributes,
      [
        { modelName: 'WorkflowAttrEntity', name: 'attr1', value: 'Value 1' },
        { modelName: 'WorkflowAttrEntity', name: 'attr2', value: 'Value 2' },
        { modelName: 'WorkflowAttrEntity', name: 'attr3', value: 'Value 3' },
        { modelName: 'WorkflowAttrEntity', name: 'newAttr', value: 'New Value' },
      ]
    )
  })
  //check that workflow ends after going to final step (step2 -> step3)
  .then()
  .send('user1', 'TransitWorkflow', {
    name: 'workflow1',
    entityId: '000000000000000000000002',
    step: 'step3',
  }, response => {
    p.equals(response.modelName, 'TransitWorkflowResp')
    p.equals(response.state.name, 'workflow1')
    p.equals(response.state.version, 2)
    p.equals(response.state.step, 'step3')
    p.equals(response.state.active, false)
    p.deepEquals(response.state.attributes,
      [
        { modelName: 'WorkflowAttrEntity', name: 'attr1', value: 'Value 1' },
        { modelName: 'WorkflowAttrEntity', name: 'attr2', value: 'Value 2' },
        { modelName: 'WorkflowAttrEntity', name: 'attr3', value: 'Value 3' },
        { modelName: 'WorkflowAttrEntity', name: 'newAttr', value: 'New Value' },
      ]
    )
  })
  //
  .then()
  .send('user1', 'TransitWorkflow', {
    name: 'workflow1',
    entityId: '000000000000000000000002',
    step: 'step3',
  }, response => {
    p.equals(response.modelName, 'ErrorResp')
    p.equals(response.code, map.ErrorCode.invalidMessage)
    p.equals(response.message, 'Workflow is ended')
  })
};
