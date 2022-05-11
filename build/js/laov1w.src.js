
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data","description":{"R":"the data as a data frame"}},{"name":"deps","title":"Dependent Variable","type":"Variable","required":true,"suggested":["continuous"],"permitted":["numeric"],"description":{"ui":"the dependent variable for ANOVA, will be continuous.\n","R":"a string naming the dependent variables in `data`\n"}},{"name":"group","title":"Grouping Variable","type":"Variable","required":true,"rejectUnusedLevels":true,"suggested":["nominal","ordinal"],"permitted":["factor"],"description":{"ui":"the explanatory or independent variable. For ANOVA this will be categorical.\n","R":"a string naming the grouping or independent variable in `data`\n"}},{"name":"con1","title":"Contrast 1","type":"Variable","required":false,"suggested":["continuous"],"permitted":["numeric"]},{"name":"con2","title":"Contrast 2","type":"Variable","required":false,"suggested":["continuous"],"permitted":["numeric"]},{"name":"desc","title":"Descriptives table","type":"Bool","default":false,"description":{"ui":"provide descriptives for each group.\n","R":"`TRUE` or `FALSE` (default), provide descriptive statistics\n"}},{"name":"descPlot","title":"Descriptives plot","type":"Bool","default":false,"description":{"ui":"provide descriptive plots for each group.\n","R":"`TRUE` or `FALSE` (default), provide descriptive plots\n"}},{"name":"norm","title":"Normality test","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), perform Shapiro-Wilk test of normality\n"}},{"name":"qq","title":"Q-Q Plot","type":"Bool","default":false,"description":{"ui":"provide Q-Q plots of residuals -- a plot of the actual residuals against what would be expected if the data were *perfectly* normally distributed. Large deviations from the diagonal line suggest the data is not from a normal distribution.\n","R":"`TRUE` or `FALSE` (default), provide a Q-Q plot of residuals\n"}},{"name":"eqv","title":"Homogeneity test","type":"Bool","default":false,"description":{"ui":"provide Levene's tests for the homogeneity of variances. A low p-value suggests the groups have unequal variances. A high S value suggests that either the group variances are unequal or their  variances are closer than you would expect.\n","R":"`TRUE` or `FALSE` (default), perform Levene's test for homogeneity of variances\n"}}];

const view = function() {
    
    this.handlers = { }

    View.extend({
        jus: "3.0",

        events: [

	]

    }).call(this);
}

view.layout = ui.extend({

    label: "One-Way ANOVA",
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
							name: "deps",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Grouping Variable",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "group",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Contrast 1",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "con1",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Contrast 2",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "con2",
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
			stretchFactor: 1,
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
							label: "Additional Statistics",
							controls: [
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "desc"
								},
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "descPlot"
								}
							]
						},
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "Assumption Checks",
							controls: [
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "eqv"
								},
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "norm"
								},
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "qq"
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
