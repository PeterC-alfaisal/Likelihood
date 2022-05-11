
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data"},{"name":"dep","title":"Dependent Variable","type":"Variable","required":true,"suggested":["continuous"],"permitted":["numeric"],"rejectInf":false},{"name":"nul","title":"Null hypothesis difference value","type":"Number","min":-100000000000,"max":100000000000,"default":0},{"name":"alt","title":"Alternative hypothesis difference value","type":"Number","min":-100000000000,"max":100000000000,"default":0},{"name":"lint","type":"Number","title":"Likelihood interval support level","min":1,"max":10,"default":2},{"name":"dtab","title":"Descriptives","type":"Bool","default":false},{"name":"plt","title":"Descriptives Plot","type":"Bool","default":false},{"name":"pll","title":"Likelihood function","type":"Bool","default":false}];

const view = function() {
    
    this.handlers = { }

    View.extend({
        jus: "3.0",

        events: [

	]

    }).call(this);
}

view.layout = ui.extend({

    label: "LR One Sample t-Test",
    jus: "3.0",
    type: "root",
    stage: 0, //0 - release, 1 - development, 2 - proposed
    controls: [
		{
			type: DefaultControls.VariableSupplier,
			typeName: 'VariableSupplier',
			persistentItems: false,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Dependent Variable",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "dep",
							maxItemCount: 1,
							isTarget: true
						}
					]
				}
			]
		},
		{
			type: DefaultControls.LayoutBox,
			typeName: 'LayoutBox',
			margin: "large",
			controls: [
				{
					type: DefaultControls.LayoutBox,
					typeName: 'LayoutBox',
					cell: {"column":0,"row":0},
					stretchFactor: 1,
					controls: [
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "Settings",
							controls: [
								{
									type: DefaultControls.LayoutBox,
									typeName: 'LayoutBox',
									margin: "large",
									controls: [
										{
											type: DefaultControls.TextBox,
											typeName: 'TextBox',
											name: "nul",
											format: FormatDef.number
										},
										{
											type: DefaultControls.TextBox,
											typeName: 'TextBox',
											name: "alt",
											format: FormatDef.number
										},
										{
											type: DefaultControls.TextBox,
											typeName: 'TextBox',
											name: "lint",
											format: FormatDef.number
										},
										{
											type: DefaultControls.LayoutBox,
											typeName: 'LayoutBox',
											margin: "large",
											controls: [
												{
													type: DefaultControls.CheckBox,
													typeName: 'CheckBox',
													name: "pll"
												}
											]
										}
									]
								}
							]
						}
					]
				},
				{
					type: DefaultControls.LayoutBox,
					typeName: 'LayoutBox',
					cell: {"column":1,"row":0},
					stretchFactor: 0,
					controls: [
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "Descriptives",
							controls: [
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "dtab"
								},
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "plt"
								}
							]
						}
					]
				}
			]
		}
	]
});

module.exports = { view : view, options: options };
