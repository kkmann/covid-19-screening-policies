function get_policy(name, lfd)
	if name == "default" 
		return SymptomaticIsolation(
			lfd, pcr_turnaround = 2, isolation_duration = 10
		)
	elseif name == "Thu/Fri off" 
		return SymptomaticIsolation(
			lfd, pcr_turnaround = 2, isolation_duration = 10, 
			fixed_isolation_weekdays = [3, 4]
		)
	elseif name == "Mon screening"
		return SymptomaticIsolation(
			lfd, pcr_turnaround = 2, isolation_duration = 10, 
			screening_test_weekdays = [0]
		)
	elseif name == "Mon/Wed screening"
		return SymptomaticIsolation(
			lfd, pcr_turnaround = 2, isolation_duration = 10, 
			screening_test_weekdays = [0, 2]
		)
	elseif name == "test & release" 
		return DynamicScreening(
			lfd, pcr_turnaround = 2, isolation_duration = 10, 
			followup_duration = 7
		)
	end
	throw(InexactError("policy not found"))
end